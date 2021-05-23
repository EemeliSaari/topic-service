{-# LANGUAGE DataKinds #-}

module Topics.Lda
( LdaSpec(..)
, ldaSpecDefaults
, initLda
, fit
, Lda
) where

import Data.List as L ( transpose )
import Control.Monad.State
    ( replicateM, MonadIO(liftIO), MonadState(put, get), StateT )
import Data.Matrix as M
    ( Matrix(..),
      fromList,
      getElem,
      safeSet,
      toList,
      toLists,
      transpose )
--import System.Random
import Debug.Trace ( traceM )
import Data.Maybe ( fromMaybe )
import Numeric.Utils ( diff, vecMatDot, outerProduct )
import Numeric.Psi ( dirichletExpectation )
import Data.Defaults ( Required, Purpose(Defaults, Complete) )
import Topics.Vocab ( bow )
import Statistics.Distribution.Gamma (gammaDistr)
import Statistics.Distribution (genContVar)
import System.Random.MWC ( asGenST, withSystemRandom )


data LdaSpec a = LdaSpec
    { ntopics :: !(Required a Int)
    , nterms :: !(Required a Int)
    , alpha :: !(Maybe Double)
    , eta :: !(Maybe Double)
    , tau :: !Double
    , kappa :: !Double
    , iter :: !Int
    , passes :: !Int
    , seed :: !(Maybe Int)
    , convergence :: !Double
    , nchunks :: !Int
    }

ldaSpecDefaults :: LdaSpec 'Defaults
ldaSpecDefaults = LdaSpec
    -- Required parameters
    { ntopics = ()
    , nterms = ()
    -- Optional parameters
    , alpha = Nothing
    , eta = Nothing
    , tau = 0.5
    , kappa = 1
    , iter = 100
    , passes = 1
    , seed = Nothing
    , convergence = 1E-3
    , nchunks = 1
    }

data Lda = Lda
    { _eta :: Double
    , _alpha :: Double
    , _lambda :: Matrix Double
    , _expElogBeta :: Matrix Double
    , _tau :: Double
    , _kappa :: Double
    , _ndocs :: Int
    , _updated :: Int
    , _maxPasses :: Int
    , _k :: Int
    , _w :: Int
    , _chunkSize :: Int
    , _convergence :: Double
    , _maxIter :: Int
    , _pass :: Int
    } deriving (Show)

type LdaState a = StateT Lda IO a

data Coordinate = Coordinate Double Int Int deriving (Show)

coordSum :: Matrix Double -> Coordinate -> Matrix Double
coordSum x (Coordinate v r c) = new
    where
        e = M.getElem r c x + v
        new = Data.Maybe.fromMaybe x (safeSet e (r, c) x)

initLda :: LdaSpec 'Complete -> IO Lda
initLda s = do
    st' <- genGamma n 100 0.01

    let eeb' = map exp x where x = dirichletExpectation st'

    return Lda
        { _eta = Data.Maybe.fromMaybe auto (eta s)
        , _alpha = Data.Maybe.fromMaybe auto (alpha s)
        , _lambda = M.fromList (ntopics s) (nterms s) st'
        , _expElogBeta = M.fromList (ntopics s) (nterms s) eeb'
        , _ndocs = 0
        , _updated = 0
        , _tau = tau s
        , _kappa = kappa s
        , _maxPasses = passes s
        , _k = ntopics s
        , _w = nterms s
        , _chunkSize = nchunks s
        , _convergence = convergence s
        , _maxIter = iter s
        , _pass = 0
        } where
            n = nterms s * ntopics s
            auto = 1 / fromIntegral (ntopics s)

elogBeta :: Matrix Double -> Matrix Double
elogBeta mat = M.fromList n m eb
    where
        n = M.nrows mat
        m = M.ncols mat
        eb = dirichletExpectation (M.toList mat)

expElogBeta :: Matrix Double -> Matrix Double
expElogBeta mat = M.fromList n m eeb
    where
        n = M.nrows mat
        m = M.ncols mat
        eeb = map exp (M.toList (elogBeta mat))

genGamma :: Int -> Double -> Double -> IO [Double]
genGamma n k t = do
    withSystemRandom . asGenST $ \g -> replicateM n $ genContVar (gammaDistr k t) g

rho :: LdaState Double
rho = do
    model <- get
    let tau' = _tau model
        nupdate' = fromIntegral (_updated model)
        kappa' = _kappa model
        chunks = fromIntegral (_chunkSize model)

    return ((tau' + nupdate'/chunks)**(-kappa'))

optimize :: Lda
    -> [Double] -- Word counts
    -> [Double] -- gammad
    -> [Double] -- expElogTheta
    -> [[Double]] -- expElogBeta
    -> Maybe [Double] -- Iterator break condition
    -> Int -- Iterator count
    -> ([Double], [[Double]])
optimize model cts g et b p i
        | converged || i >= _maxIter model = (g', outerProduct et' tCnt)
        | otherwise = optimize model cts g' et' b (Just newP) (i + 1)
    where
        a = _alpha model
        p' = case p of
            Just x -> x
            Nothing -> map (+ 0.000001) (vecMatDot et b)
        tCnt = vecMatDot [x/y|(x, y) <- zip cts p'] (L.transpose b)
        g' = [a + y*z|(y, z) <- zip et tCnt]
        et' = map exp (dirichletExpectation g')
        newP =  map (+ 0.0000001) (vecMatDot et' b)
        converged = (i > 0) && (diff g g' > _convergence model)

subStep :: Lda -> [Double] -> Matrix Double -> [(Int, Int)] -> ([Double], [[Double]])
subStep model gammad et doc = optimize model counts gammad expElogT expElogB Nothing 0
    where
        elogT = dirichletExpectation gammad
        expElogT = map exp elogT
        ids = [x | (x, _) <- doc]
        counts = [fromIntegral y | (_, y) <- doc]
        expElogB = map (\i -> M.toLists (M.transpose et)!!i) ids

-- Training iteration
updateLambda :: [[Int]] -> LdaState ()-- Lda
updateLambda c = do
    model <- get

    let topics = _k model
        batch = length c
        expElogB = expElogBeta (_lambda model)

    gamma <- liftIO $ genGamma (batch*topics) 100 0.01

    let docs = map bow c
        results = [subStep model (slice i gamma) expElogB d | (d, i) <- zip docs [0..]]
            where
                slice :: Int -> [Double] -> [Double]
                slice idx mat
                    | idx >= from + topics = error "NOOOO"
                    | from+topics-1 >= length mat = error ("NEEEEJ " ++ show idx ++ ", " ++ show mat ++ ", " ++ show topics ++ ", " ++ show from ++ ", " ++ show docs)
                    | otherwise = [mat!!i | i <- [from..(from+topics-1)]]
                    where
                        from = idx*topics

    -- Generator to slice correct term|topic results
    let docsstats = do
            ((_, res), doc) <- zip results docs
            rows <- [(zip x doc, i) | (x, i) <- zip res [0..]]
            let (row, idx) = rows
            (value, (idy, _)) <- row
            return (Coordinate value idx idy)

    let res' = foldl coordSum expElogB docsstats
        sstats' = [x*y | (x, y) <- zip (M.toList res') (M.toList expElogB)]
        sstats = M.fromList (M.nrows res') (M.ncols res') sstats'

    rhot <- rho
    traceM $ "sstats: " ++ show sstats
    let oldLambda = _lambda model
        eta' = _eta model
        n = fromIntegral (length c)

    let a = [x*(1-rhot) + rhot*(eta'+100.0*y/n) | (x, y) <- zip (M.toList oldLambda) (M.toList sstats)]
        newLambda = M.fromList (M.nrows sstats) (M.ncols sstats) a

    put model { _lambda = newLambda }

-- Main training loop
fit :: [[Int]] -> LdaState Lda
fit c = do
    updateLambda c
    model <- get
    put model { _pass = _pass model + 1 }
    --traceM $ show model
    if _maxPasses model > _pass model
        then
            fit c
    else
        return model

{-# LANGUAGE DataKinds #-}

module Topics.Lda
( LdaSpec(..)
, ldaSpecDefaults
, initLda
, update
, Lda
) where

import Data.List (transpose)
import Control.Monad.State
import Data.Matrix as M hiding (transpose, trace)
import Data.Vector as V (toList)
--import Numeric.Limits (epsilon)
import System.Random
import Debug.Trace

import Numeric.Psi
import Numeric.Gamma
import Numeric.Utils
import Data.Defaults
import Stats.Gamma
import Topics.Vocab

data LdaSpec a = LdaSpec 
    { ntopics :: !(Required a Int)
    , nterms :: !(Required a Int)
    , alpha :: !(Maybe Double)
    , eta :: !(Maybe Double)
    , decay :: !Double
    , iter :: !Int
    , passes :: !Int
    , offset :: !Double
    , seed :: !(Maybe Int)
    , convergence :: Double
    }

ldaSpecDefaults :: LdaSpec Defaults
ldaSpecDefaults = LdaSpec 
    -- Required parameters
    { ntopics = ()
    , nterms = ()
    -- Optimal parameters
    , alpha = Nothing
    , eta = Nothing
    , decay = 0.5
    , iter = 100
    , passes = 10
    , offset = 1
    , seed = Nothing
    , convergence = 1E-3
    }

data Lda = Lda
    { _eta :: [Double]
    , _alpha :: [Double]
    , _sstats :: Matrix Double
    , _expElogBeta :: Matrix Double
    , _ndocs :: Int
    , _pass :: Int
    , _offset :: Double
    , _decay :: Double
    , _numUpdate :: Int
    , _maxPasses :: Int
    , _numTopics :: Int
    , _chunkSize :: Maybe Int
    , _history :: [Double]
    , _generator :: StdGen
    , _convergence :: Double
    , _maxIter :: Int
    } deriving (Show)

type LdaState = State Lda

--storeState :: LdaState -> IO Bool
--storeState s = do
--    return True

loadState :: Monad m => String -> m Lda
loadState p = do
    return Lda{}

initLda :: LdaSpec Complete -> IO Lda
initLda s = do
    randomSeed <- randomIO
    let g = mkStdGen randomSeed

    let st' = take n (gammaSample 100 0.01 g)
    let eeb' = map exp x where x = dirichletExpectation st'

    return Lda
        { _eta = replicate (nterms s) (case (eta s) of
            Just x -> x
            Nothing -> auto)
        , _alpha = replicate (ntopics s) (case (alpha s) of
            Just x -> x
            Nothing -> auto)
        , _sstats = M.fromList (ntopics s) (nterms s) st'
        , _expElogBeta = M.fromList (ntopics s) (nterms s) eeb'
        , _ndocs = 0
        , _pass = 0
        , _numUpdate = 0
        , _offset = offset s
        , _decay = decay s
        , _maxPasses = passes s
        , _numTopics = ntopics s
        , _chunkSize = Nothing
        , _history = []
        , _generator = g
        , _convergence = convergence s
        , _maxIter = iter s
        } where
            n = (nterms s) * (ntopics s)
            auto = 1 / fromIntegral (ntopics s)

genGamma :: Int -> Double -> Double -> LdaState [Double]
genGamma n k t = do
    model <- get
    let g = _generator model
        results = take n (gammaSample k t g)
        (_, new) = next g
    put model {_generator = new}
    return results

rho :: LdaState Double
rho = do
    s <- get
    let offset' = (_offset s)
        npasses' = fromIntegral (_pass s)
        nupdate' = fromIntegral (_numUpdate s)
        decay' = (_decay s)

    let chunks = fromIntegral (case (_chunkSize s) of 
                    Just 0 -> error "chunkSize must be larger than 0"
                    Just x -> x
                    Nothing -> error "No chunkSize set for LDA")
    let res = (offset' + npasses' + nupdate'/chunks)**(-decay')
    return res

optimize :: Lda
         -> [Double]
         -> [Double]
         -> [Double]
         -> [[Double]]
         -> Maybe [Double]
         -> Int
         -> ([Double], [[Double]])
optimize model cts g et b p i
        | converged || i >= (_maxIter model) = (g', (outerProduct et' tCnt)) 
        | otherwise = optimize model cts g' et' b (Just newP) (i + 1)
    where
        a = (_alpha model)
        p' = case p of
            Just x -> x
            Nothing -> map (\y -> y + 0.000001) (vecMatDot et b)
        tCnt = vecMatDot [x/y|(x, y) <- zip cts p'] (transpose b)
        g' = [x + y*z|(x, y, z) <- zip3 a et tCnt]
        et' = map exp (dirichletExpectation g')
        newP =  map (\y -> y + 0.000001) (vecMatDot et' b)
        converged = if i > 0 then diff g g' > (_convergence model) else False

inferenceStep :: Lda -> Matrix Double -> [(Int, Int)] -> ([Double], [[Double]])
inferenceStep model et doc = optimize model counts gammad expElogT expElogB Nothing 0
    where
        gammad = take (_numTopics model) (gammaSample 100 0.01 (_generator model))
        elogT = dirichletExpectation gammad
        expElogT = map exp elogT
        ids = [x | (x, y) <- doc]
        counts = [fromIntegral y | (x, y) <- doc]
        expElogB = map (\i -> (transpose (M.toLists et))!!i) ids
        alpha' = _alpha model

--collectStats :: Ord a => [(a, Int)] -> [[Double]] -> LdaState (Matrix Double)
--collectStats doc dsstats = do
--    model <- get
--    return (_sstats model)

data Coordinate = Coordinate Double Int Int deriving (Show)

inference :: [[Int]] -> LdaState (Matrix Double, Matrix Double)
inference c = do
    model <- get
    expElogB <- expElogBeta
    let docs = map bow c
        results = map (\d -> inferenceStep model expElogB d) docs
        gammas = [x | (x, y) <- results]
        org = (_sstats model)
        -- Generator to slice correct term|topic results
        docsstats = do
            ((_, res), doc) <- zip results docs
            rows <- [(zip x doc, i) | (x, i) <- zip res [0..]]

            let (row, idx) = rows
            (value, (idy, _)) <- row

            return (Coordinate value idx idy)

    let sstats = foldl (coordSum) expElogB docsstats

    return ((M.fromLists gammas), expElogB)


coordSum :: Matrix Double -> Coordinate -> Matrix Double
coordSum x (Coordinate v r c) = new
    where
        elem = (M.getElem r c x) + v
        new = case (M.safeSet elem (r, c) x) of 
            Just m -> m
            Nothing -> x


lambda :: LdaState (Matrix Double)
lambda = do
    s <- get
    let eta' = (_eta s)
    let sstate = (_sstats s)
    return (colSum sstate eta' 0)
        where
            colSum x v i
                | i < (M.ncols x) = colSum x' v (i + 1)
                | otherwise = x
                where
                    x' = M.mapCol (\_ y -> y + (v!!i)) i x

elogBeta :: LdaState (Matrix Double)
elogBeta = do
    lmd <- lambda
    return (M.fromLists (map dirichletExpectation (mats lmd)))
        where
            mats x = map (\i -> (V.toList (M.getRow i x))) [1..(M.nrows x)]

expElogBeta :: LdaState (Matrix Double)
expElogBeta = do
    mat <- elogBeta
    let n = M.nrows mat
        m = M.ncols mat
        new = M.fromList n m [exp x|x <- M.toList mat]
    return new

blend :: Matrix Double -> LdaState (Matrix Double)
blend x = do
    model <- get
    lr <- rho
 
    let sstats = (_sstats model)
        n = M.nrows x
        m = M.ncols x
        scale = 1
        op a b rho = a*((1 - rho)*scale) + (rho*scale*b)
        new = (M.fromList n m [op x' y' lr|(x', y') <- zip (M.toList x) (M.toList sstats)])

    put model {_sstats = new }

    return new

updateAlpha :: Matrix Double -> LdaState [Double]
updateAlpha gam = do
    s <- get
    lr <- rho
    let logphat = map (\x -> sum (dirichletExpectation (V.toList (M.getRow x gam)))/n) [1..(M.nrows gam)]
    let alpha' = dirichletPrior (_alpha s) n logphat lr
    return alpha'
        where 
            n = fromIntegral (M.nrows gam)

updateState :: [[Int]] -> LdaState ()
updateState c = do
    model <- get
    oldExpElog <- expElogBeta

    put model { _chunkSize = Just (length c) }

    (gammat, sstats) <- inference c
    --traceShowM sstats
    alp <- updateAlpha gammat
    sstats' <- blend (sstats * oldExpElog)
    newExpElog <- expElogBeta

    put model { _pass = (_pass model) + 1
              , _alpha = alp
              , _sstats = sstats'
              , _history = (_history model) ++ [diff (M.toList oldExpElog) (M.toList newExpElog)]
              , _chunkSize = Just (length c)
              }

-- Main loop
update :: [[Int]] -> LdaState Lda
update c = do
    updateState c
    s <- get
    if (_maxPasses s) > (_pass s)
        then do 
            update c
    else do 
        return s

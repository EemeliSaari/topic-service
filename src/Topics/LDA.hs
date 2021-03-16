{-# LANGUAGE DataKinds #-}

module Topics.Lda
( LdaSpec(..)
, ldaSpecDefaults
, initLda
, fit
, Lda
) where

import Data.List as L
import Control.Monad.State
import Data.Matrix as M hiding (trace)
import Data.Vector as V (toList)
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
    , tau :: !Double
    , kappa :: !Double
    , iter :: !Int
    , passes :: !Int
    , seed :: !(Maybe Int)
    , convergence :: !Double
    , nchunks :: !Int
    }

ldaSpecDefaults :: LdaSpec Defaults
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
    , passes = 10
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
    , _generator :: StdGen
    } deriving (Show)

type LdaState = State Lda

data Coordinate = Coordinate Double Int Int deriving (Show)

coordSum :: Matrix Double -> Coordinate -> Matrix Double
coordSum x (Coordinate v r c) = new
    where
        e = (M.getElem r c x) + v
        new = case (M.safeSet e (r, c) x) of 
            Just m -> m
            Nothing -> x

initLda :: LdaSpec Complete -> IO Lda
initLda s = do
    randomSeed <- randomIO
    let g = mkStdGen randomSeed
        st' = take n (gammaSample 100 0.01 g)
        eeb' = map exp x where x = dirichletExpectation st'

    return Lda
        { _eta = case (eta s) of
            Just x -> x
            Nothing -> auto
        , _alpha = case (alpha s) of
            Just x -> x
            Nothing -> auto
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
        , _generator = g
        , _convergence = convergence s
        , _maxIter = iter s
        , _pass = 0
        } where
            n = (nterms s) * (ntopics s)
            auto = 1 / fromIntegral (ntopics s)

elogBeta :: LdaState (Matrix Double)
elogBeta = do
    model <- get
    let eb = dirichletExpectation (M.toList (_lambda model))
    return (M.fromList (_k model) (_w model) eb)

expElogBeta :: LdaState (Matrix Double)
expElogBeta = do
    model <- get
    eb <- elogBeta
    let eeb = map exp (M.toList eb)
    return (M.fromList (_k model) (_w model) eeb)

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
    model <- get
    let tau' = (_tau model)
        nupdate' = fromIntegral (_updated model)
        kappa' = (_kappa model)
        chunks = fromIntegral (_chunkSize model) 

    return ((tau' + nupdate'/chunks)**(-kappa'))

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
        tCnt = vecMatDot [x/y|(x, y) <- zip cts p'] (L.transpose b)
        g' = [a + y*z|(y, z) <- zip et tCnt]
        et' = map exp (dirichletExpectation g')
        newP =  map (\y -> y + 0.0000001) (vecMatDot et' b)
        converged = if i > 0 then diff g g' > (_convergence model) else False

subStep :: Lda -> [Double] -> Matrix Double -> [(Int, Int)] -> ([Double], [[Double]])
subStep model gammad et doc = optimize model counts gammad expElogT expElogB Nothing 0
    where
        elogT = dirichletExpectation gammad
        expElogT = map exp elogT
        ids = [x | (x, _) <- doc]
        counts = [fromIntegral y | (_, y) <- doc]
        expElogB = map (\i -> (L.transpose (M.toLists et))!!i) ids

estep :: [[Int]] -> LdaState (Matrix Double, Matrix Double)
estep c = do
    model <- get

    let topics = _k model
        batch = length c

    gamma <- genGamma (batch*topics) 100 0.01
    expElogB <- expElogBeta

    let gammaB = M.fromList (batch) (topics) gamma
        docs = map bow c
        results = [subStep model (V.toList (M.getRow i gammaB)) expElogB d | (d, i) <- zip docs [0..]]
        gammas = [x | (x, _) <- results]

    -- Generator to slice correct term|topic results
    let docsstats = do
            ((_, res), doc) <- zip results docs
            rows <- [(zip x doc, i) | (x, i) <- zip res [0..]]
            let (row, idx) = rows
            (value, (idy, _)) <- row
            return (Coordinate value idx idy)
    
    let sstats = (foldl (coordSum) expElogB docsstats) * expElogB

    return ((M.fromLists gammas), sstats)

-- Training iteration
updateLambda :: [[Int]] -> LdaState Lda
updateLambda c = do
    model <- get
    (gamma, sstats) <- estep c
    rhot <- rho

    let oldLambda = _lambda model
        eta = _eta model
        n = fromIntegral (length c)

    let a = [x*(1-rhot) + rhot*(eta+100.0*y/n) | (x, y) <- zip (M.toList oldLambda) (M.toList sstats)]
        newLambda = M.fromList (M.nrows sstats) (M.ncols sstats) a 

    put model { _lambda = newLambda, _pass = ((_pass model) + 1)}

    return model

-- Main training loop
fit :: [[Int]] -> LdaState Lda
fit c = do
    updateLambda c
    model <- get
    if (_maxPasses model) > (_pass model)
        then do 
            fit c
    else do 
        return model

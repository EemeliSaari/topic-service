{-# LANGUAGE DataKinds #-}

module Topics.Lda
( LdaSpec(..)
, ldaSpecDefaults
, initLda
, update
, Lda
) where

import Control.Monad
import System.Random.MWC
import System.Random.MWC.Distributions (gamma)
import Control.Monad.State
import Data.Matrix as M

import Stats.Psi
import Data.Defaults


data LdaSpec a = LdaSpec 
    { ntopics :: !(Required a Int)
    , nterms :: !(Required a Int)
    , alpha :: !(Maybe Double)
    , eta :: !(Maybe Double)
    , decay :: !Double
    , iter :: !Int
    , passes :: !Int
    , offset :: !Double
    , seed :: !Int
    }


ldaSpecDefaults :: LdaSpec Defaults
ldaSpecDefaults = LdaSpec 
    -- Required parameters
    { ntopics = ()
    , nterms = ()
    -- Optimal parameters
    , alpha = Nothing
    , eta = Nothing
    , decay = 0.1
    , iter = 10
    , passes = 5
    , offset = 1
    , seed = 42
    }


data Lda = Lda
    { _eta :: [Double]
    , _alpha :: [Double]
    , _sstate :: Matrix Double
    , _expElogBeta :: Matrix Double
    , _ndocs :: Int
    , _pass :: Int
    , _offset :: Double
    , _decay :: Double
    , _numUpdate :: Int
    , _maxPasses :: Int
    , _numTopics :: Int
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
    g <- createSystemRandom
    st' <- replicateM n $ gamma 100 0.01 g
    let eeb' = map exp x where x = dirichletExpectation st'
    return Lda
        { _eta = replicate (nterms s) (case (eta s) of
            Just x -> x
            Nothing -> auto)
        , _alpha = replicate (ntopics s) (case (alpha s) of
            Just x -> x
            Nothing -> auto)
        , _sstate = M.fromList (ntopics s) (nterms s) st'
        , _expElogBeta = M.fromList (ntopics s) (nterms s) eeb'
        , _ndocs = 0
        , _pass = 0
        , _numUpdate = 0
        , _offset = offset s
        , _decay = decay s
        , _maxPasses = passes s
        , _numTopics = ntopics s
        } where
            n = (nterms s) * (ntopics s)
            auto = 1 / fromIntegral (ntopics s)

-- Learning rate
rho :: Lda -> Double -> Double
rho st cs = ((_offset st) + ps + (nupd / cs)) ** (-(_decay st)) :: Double
    where
        ps = fromIntegral (_pass st)
        nupd = fromIntegral (_numUpdate st)

dirichletExpectation :: [Double] -> [Double]
dirichletExpectation a = map (\x -> (psi x) - y) a
    where
        y = psi (sum a)

inferenceStep :: [Int] -> [Double]
inferenceStep d = [1.0, 4.5]
    where
        g = [0.5, 0.58, 0.34] --GENERATE
        et = dirichletExpectation g
        et' = map exp et

inference :: [[Int]] -> (Matrix Double, Matrix Double)
inference c = (M.zero 5 3, M.zero 5 3)

--lambda :: LdaState -> Matrix Double
--lambda st = (_eta st + _sstate st)

updateStep :: Monad m => [[Int]] -> m Lda -> Lda
updateStep c s = do
    return s { _pass = newPass , _alpha = (_alpha s)} 
        where
            newPass = (_pass s) + 1

updateState :: [[Int]] -> LdaState Lda
updateState c = state (\st -> let st' = (updateStep c st) in (st', st'))

-- Main loop
update :: [[Int]] -> LdaState Lda
update c = do
    s <- updateState c
    if (_maxPasses s) > (_pass s)
        then do 
            update c
    else do 
        return s

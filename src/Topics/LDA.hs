module Topics.Lda
( LdaSpec(..)
, initLda
, update
, Lda
) where

import Stats.Psi
import Control.Monad.State
import Data.Matrix as M
import Data.Vector as V hiding (update, sum, foldl, length, map)


data LdaSpec = LdaSpec 
    { ntopics :: Int
    , alpha :: Double
    , eta :: Double
    , decay :: Double
    , iter :: Int
    , passes :: Int
    , nterms :: Int
    , offset :: Double
    } deriving (Show, Read)

data LdaState = LdaState
    { _eta :: Vector Double
    , _alpha :: Vector Double
    , _sstate :: Matrix Double
    , _expElogBeta :: Matrix Double
    , _ndocs :: Int
    , _pass :: Int
    , _offset :: Double
    , _decay :: Double
    , _nupdate :: Int
    } deriving (Show)

data Lda = Lda
    { spec :: LdaSpec
    , _state :: LdaState
    }

-- Show only the State
instance Show Lda where
    show (Lda _ b) = show b

type LdaMonad = State Lda

initState :: LdaSpec -> LdaState
initState s = LdaState
    { _eta = e
    , _sstate = st
    , _ndocs = 0
    , _pass = 0
    , _alpha = a
    , _offset = offset s
    , _decay = decay s
    , _nupdate = 0
    } where
        e = V.replicate (nterms s) (eta s)
        a = V.replicate (ntopics s) (alpha s)
        st = M.zero (ntopics s) (nterms s)

initLda :: LdaSpec -> Lda
initLda s = Lda
    { spec = s
    , _state = initState s
    }

-- Learning rate
rho :: LdaState -> Double -> Double
rho st cs = ((_offset st) + ps + (nupd / cs)) ** (-(_decay st)) :: Double
    where
        ps = fromIntegral (_pass st)
        nupd = fromIntegral (_nupdate st)

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

eStep :: LdaState -> [[Int]] -> Matrix Double
eStep st c = gamma
    where
        (gamma, sstate) = inference c

--lambda :: LdaState -> Matrix Double
--lambda st = (_eta st + _sstate st)

updateStep :: [[Int]] -> Lda -> Lda
updateStep c m = m {_state = newState}
    where
        oldState = (_state m)
        newPass = (_pass oldState) + 1
        newState = oldState
            { _pass = newPass
            }

updateState :: [[Int]] -> LdaMonad Lda
updateState c = state (\st -> let st' = (updateStep c st) in (st', st'))

-- Main loop
update :: [[Int]] -> LdaMonad Lda
update c = do
    s <- updateState c
    if ((passes (spec s)) > (_pass (_state s)))
        then do 
            update c
    else do 
        return s

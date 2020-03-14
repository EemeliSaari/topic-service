module Topics.Lda
( LdaSpec(..)
, initLda
, update
, Lda
) where

import System.Random
import Control.Monad.State
import Data.Matrix as M
import Data.Vector as V hiding (update, sum, foldl, length)


data LdaSpec = LdaSpec 
    { ntopics :: Int
    , alpha :: Double
    , eta :: Double
    , decay :: Double
    , iter :: Int
    , passes :: Int
    , nterms :: Int
    } deriving (Show, Read)

data LdaState = LdaState
    { _eta :: Vector Double
    , _alpha :: Vector Double
    , _sstate :: Matrix Double
    , _ndocs :: Int
    , _pass :: Int
    } deriving (Show)

data Lda = Lda
    { spec :: LdaSpec
    , _state :: LdaState
    }

-- Show only the State
instance Show Lda where
    show (Lda a b) = show b

type LdaMonad = State Lda

initState :: LdaSpec -> LdaState
initState s = LdaState
    { _eta = e
    , _sstate = st
    , _ndocs = 0
    , _pass = 0
    , _alpha = a
    } where
        e = V.replicate (nterms s) (eta s)
        a = V.replicate (ntopics s) (alpha s)
        st = M.zero (ntopics s) (nterms s)

initLda :: LdaSpec -> Lda
initLda s = Lda
    { spec = s
    , _state = initState s
    }

valFromState :: Lda -> Lda
valFromState s = s

updateStep :: [[Int]] -> Lda -> Lda
updateStep c m = m { _state = newState }
    where
        os = (_state m)
        s = _pass os
        nd = (_ndocs os) + length(c)
        newState = os {_pass = s, _ndocs = nd}

stateStep :: [[Int]] -> LdaMonad Lda
stateStep c = state (\st -> let st' = (updateStep c st) in (valFromState st', st'))

-- Main loop
update :: [[Int]] -> Int -> LdaMonad Lda
update c n = do
    s <- stateStep c
    if ((iter (spec s) - 1) > n)
        then do update c (n + 1)
    else do return s

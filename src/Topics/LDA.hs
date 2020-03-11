module Topics.Lda
( LdaSpec(..)
, initLda
) where

import System.Random
import Control.Monad.State
import Data.Matrix as M
import Data.Vector as V


data LdaSpec = LdaSpec 
    { ntopics :: Int
    , alpha :: Double
    , eta :: Double
    , decay :: Double
    , iter :: Int
    , nterms :: Int
    } deriving (Show, Read)

data LdaState = LdaState
    { _eta :: Vector Double
    , _sstate :: Matrix Double
    , _ndocs :: Int
    } deriving (Show)

type LdaStateMonad = State LdaState

initState :: LdaSpec -> LdaState
initState s = LdaState
    { _eta = e
    , _sstate = st
    , _ndocs = 0
    } where
        e = V.replicate (nterms s) (eta s)
        st = M.zero (ntopics s) (nterms s)

data Lda = Lda
    { spec :: LdaSpec
    , _alpha :: Vector Double
    , _state :: LdaState
    } deriving (Show)

initLda :: LdaSpec -> Lda
initLda s = Lda
    { spec = s
    , _alpha = a
    , _state = initState s
    }
    where
        a = V.replicate (ntopics s) (alpha s)
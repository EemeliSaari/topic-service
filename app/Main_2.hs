module Main where

import Control.Monad.State
import System.Random (StdGen, randomR, newStdGen)

import Debug.Trace

randomDist :: (RandomGen g, Random a, Num a) => (a -> a) -> State g a
randomDist distInv = liftM distInv (State (randomR (0,1)))

main :: IO ()
main = do
    evalState (randomDist distInv) (mkStdGen an_arbitrary_seed)

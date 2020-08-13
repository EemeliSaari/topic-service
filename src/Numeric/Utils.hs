module Numeric.Utils
( mean
, diff
, dirichletPrior
, dirichletExpectation
, dot
, vecMatDot
, outerProduct
) where

import Data.List (transpose) 
import Numeric.Psi
import Numeric.Gamma

mean :: [Double] -> Double
mean v = (sum v)/(n)
    where
        n = fromIntegral (length v)

diff :: [Double] -> [Double] -> Double
diff x y = mean [abs (a - b)|(a, b) <- zip x y]

-- J. Huang: "Maximum Likelihood Estimation of Dirichlet Distribution Parameters
dirichletPrior :: [Double] -> Double -> [Double] -> Double -> [Double]
dirichletPrior p n lp r 
        | all (>0) dp = zipWith (\x y -> x*r + y) dp p
        | otherwise = p
    where
        psum = n * (psi (sum p))
        gradf = zipWith (\x y -> psum - x + y) p lp
        c = n * (polygamma 1 (sum p))
        q = map (\x -> -n * (polygamma 1 x)) p
        b = sum (zipWith (\x y -> x/y) p q) / (1/c + (sum (map (\x -> 1/x) q)))
        dp = zipWith (\x y -> -(x - b)/y) p q

dirichletExpectation :: [Double] -> [Double]
dirichletExpectation a = map (\x -> (psi x) - y) a
    where
        y = psi (sum a)

dot :: [Double] -> [Double] -> Double
dot u v = sum [x*y | (x, y) <- zip u v]

-- Ax = xA^T, where A is (m, n)-dim matrix and x is m-dim vector
vecMatDot :: [Double] -> [[Double]] -> [Double]
vecMatDot u v = [dot u b | b <- (transpose v)]

-- https://en.wikipedia.org/wiki/Outer_product
outerProduct :: [Double] -> [Double] -> [[Double]]
outerProduct u v = [map (\v' -> v'*u') v | u' <- u]

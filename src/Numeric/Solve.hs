-- Code adopted from [scipy.special.psi](https://numpy.org/doc/stable/reference/generated/numpy.polyval.html)

module Numeric.Solve
( solvePoly
, polyEval
, doubleFloor
) where

doubleFloor :: Double -> Double
doubleFloor x = fromIntegral (floor x) :: Double

solvePoly :: Double -> Double -> [Double] -> Int -> Double
solvePoly x y coef n
        | n > 0 = solvePoly x ans coef' (n - 1)
        | otherwise = ans
    where
        y' = if (null coef) then 0.0 else (head coef)
        coef' = if (null coef) then coef else (tail coef)
        ans = y * x + y'

polyEval :: Double -> [Double] -> Int -> Double
polyEval x coef n = solvePoly x (head coef) (tail coef) n 

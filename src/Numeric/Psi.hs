-- Code adopted from [scipy.special.psi](https://docs.scipy.org/doc/scipy/reference/generated/scipy.special.psi.html)

module Numeric.Psi
( psi
, psiAsy
, dirichletExpectation
) where

import Numeric.Solve ( doubleFloor, polyEval )

dirichletExpectation :: [Double] -> [Double]
dirichletExpectation a = map (\x -> psi x - y) a
    where
        y = psi (sum a)

modf :: Double -> Double
modf x = m * (x' - ip)
    where
        x' = abs x
        m = if x < 0 then -1.0 else 1.0
        ip = doubleFloor x'

digamma12 :: Double -> Double
digamma12 x = g * 0.99558162689208984 + g * r
    where
        r1 = 1569415565.0 / 1073741824.0
        r2 = (381566830.0 / 1073741824.0) / 1073741824.0
        r3 = 0.9016312093258695918615325266959189453125e-19
        g = x - r1 - r2 - r3
        r = polyEval (x - 1.0) coefP 5 / polyEval (x - 1.0) coefQ 6

psiAsy :: Double -> Double
psiAsy x = log x - (0.5 / x) - y
    where
        y   | x < 1.0e17 = z*polyEval z coefA 6
            | otherwise = 0
            where
                z = 1/x**2

-- Psi (digamma) function
psi :: Double -> Double
psi x
        | x == 0 = error msg
        | x < 0 && r == 0 = error msg
        | x < 0 && r /= 0 = solvePsi (1.0 - x) (-pi / tan (pi * r))
        | otherwise = solvePsi x 0.0
    where
        r = modf x
        msg = "Singular: (x=" ++ show x ++ ", r=" ++ show r ++ ")"

solvePsi :: Double -> Double -> Double
solvePsi x y
        | x <= 10.0 && doubleFloor x == x = easy (floor x) y
        | x < 1.0 = main (x + 1.0) (y - (1.0 / x))
        | x < 10.0 = sub x y
        | otherwise = main x y
    where
        easy :: Int -> Double -> Double
        easy n y' = y' + sum [1/fromIntegral i |i <- [1..n-1]] - eulerGamma
        sub :: Double -> Double -> Double
        sub x' y'
            | x' > 2 = sub new' (y' + (1.0 / new'))
            | otherwise = main x' y'
            where
                new' = x' - 1.0
        main :: Double -> Double -> Double
        main x' y'
            | x' <= 2.0 && x' >= 1.0 = y' + digamma12 x'
            | otherwise = y' + psiAsy x'

coefA :: [Double]
coefA = [8.33333333333333333333E-2,
         -2.10927960927960927961E-2,
         7.57575757575757575758E-3,
         -4.16666666666666666667E-3,
         3.96825396825396825397E-3,
         -8.33333333333333333333E-3,
         8.33333333333333333333E-2]

coefP :: [Double]
coefP = [-0.0020713321167745952,
         -0.045251321448739056,
         -0.28919126444774784,
         -0.65031853770896507,
         -0.32555031186804491,
         0.25479851061131551]

coefQ :: [Double]
coefQ = [-0.55789841321675513e-6,
         0.0021284987017821144,
         0.054151797245674225,
         0.43593529692665969,
         1.4606242909763515,
         2.0767117023730469,
         1.0]

eulerGamma :: Double
eulerGamma = 0.5772156649015329

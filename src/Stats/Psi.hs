-- Code adopted from [scipy.special.psi](https://docs.scipy.org/doc/scipy/reference/generated/scipy.special.psi.html)
module Stats.Psi
( psi
, solvePoly
, 
) where

polyEval :: Double -> [Double] -> Int -> Double
polyEval x coef n = solvePoly x (head coef) (tail coef) n 

doubleFloor :: Double -> Double
doubleFloor x = fromIntegral (floor x) :: Double

modf :: Double -> Double
modf x = m * (x' - ip)
    where
        x' = abs x
        m = if (x < 0) then -1.0 else 1.0
        ip = doubleFloor x'

solvePoly :: Double -> Double -> [Double] -> Int -> Double
solvePoly x y coef n
        | n > 0 = solvePoly x ans coef' (n - 1)
        | otherwise = ans
    where
        y' = if (null coef) then 0.0 else (head coef)
        coef' = if (null coef) then coef else (tail coef)
        ans = y * x + y'

digamma12 :: Double -> Double
digamma12 x = g * 0.99558162689208984 + g * r
    where
        r1 = 1569415565.0 / 1073741824.0
        r2 = (381566830.0 / 1073741824.0) / 1073741824.0
        r3 = 0.9016312093258695918615325266959189453125e-19
        g = x - (r1 + r2 + r3)
        r = (polyEval (x - 1.0) coefP 5) / (polyEval (x - 1.0) coefQ 6)

psiAsy :: Double -> Double
psiAsy x
        | x < 1.0e17 = y - (z * (polyEval z coefA 6))
        | otherwise = y
    where
        y = (log x) - (0.5 / x)
        z = 1.0 / (x**2)

-- Psi (digamma) function
psi :: Double -> Double
psi x
        | x == 0 || r == 0 = error "Singular"
        | x < 0 = solvePsi (1.0 - x) (-pi / (tan (pi * r)))
        | otherwise = solvePsi x 0.0
    where
        r = modf x

solvePsi :: Double -> Double -> Double
solvePsi x y
        | x <= 10.0 && x == doubleFloor x = 1.0
        | x < 1.0 = solvePsi (x + 1.0) (y - (1.0 / x))
        | x <= 2.0 && x >= 1.0 = y + digamma12 x
        | x < 10.0 = solvePsi (x - 1.0) (y + (1.0 / x))
        | otherwise = y + (psiAsy x)

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
    
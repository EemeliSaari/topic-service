-- Code adopted from [scipy.special.psi](https://docs.scipy.org/doc/scipy/reference/generated/scipy.special.gamma.html)

module Numeric.Gamma
( gammaf
, polygamma
, stirf
) where

import Numeric.Zeta
import Numeric.Solve

polygamma :: Int -> Double -> Double
polygamma n x = ((-1) ** nn) * (gammaf nn) * (zetaf nn x)
    where
        nn = fromIntegral (n + 1)

stirf :: Double -> Double
stirf x = y*2.50662827463100050242E0*w
    where
        w = 1 + 1/x * (polyEval (1/x) coefS 4) 
        y | x > 143.016 = v*(v/exp x)
          | otherwise = x**(x - 0.5)/exp x
            where
                v = x**(0.5 * x - 0.25)

gammaf :: Double -> Double
gammaf x
        | q > 33 = solveLarge x
        | otherwise = solve x 1
    where
        q = abs x
        solveLarge x'
            | x' < 0 && p == q = error "Overflow"
            | otherwise = gamSign * (solveSubZ (q - p))
            where
                p = doubleFloor q
                gamSign | even (toInteger (floor q)) = -1
                        | otherwise = 1
                solveSubZ z' | x >= 0 = stirf x
                             | ztmp == 0 = error "Inf"
                             | z' <= 0.5 = pi / (abs ztmp * stirf q)
                             | otherwise = solveSubZ (q - (p + 1))
                        where
                            ztmp = q*sin (pi*z') 
        solve x' z
            | x' >= 3 = solve (x' - 1) (z*(x' - 1))
            | x' < 0 = solve (x' + 1) (z/x)
            | x' < 1E-9 = z / (x + 0.5772156649015329*(x**2))
            | x' == 0 = error "Zero"
            | x' == 2 = z
            | otherwise = z*p'/q'
                where
                    p' = polyEval (x - 2) coefP 6
                    q' = polyEval (x - 2) coefQ 7

coefP :: [Double]
coefP = [1.60119522476751861407E-4,
         1.19135147006586384913E-3,
         1.04213797561761569935E-2,
         4.76367800457137231464E-2,
         2.07448227648435975150E-1,
         4.94214826801497100753E-1,
         9.99999999999999996796E-1]

coefQ :: [Double]
coefQ = [-2.31581873324120129819E-5,
         5.39605580493303397842E-4,
         -4.45641913851797240494E-3,
         1.18139785222060435552E-2,
         3.58236398605498653373E-2,
         -2.34591795718243348568E-1,
         7.14304917030273074085E-2,
         1.00000000000000000320E0]

-- Stirling's
coefS :: [Double]
coefS = [7.87311395793093628397E-4,
         -2.29549961613378126380E-4,
         -2.68132617805781232825E-3,
         3.47222221605458667310E-3,
         8.33333333333482257126E-2]

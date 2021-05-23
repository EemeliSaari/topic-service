-- Code adopted from [scipy.special.psi](https://docs.scipy.org/doc/scipy/reference/generated/scipy.special.zeta.html)

module Numeric.Zeta 
( zetaf
) where

import Numeric.Solve

zetaf :: Double -> Double -> Double
zetaf x q 
        | x <= 1 = error ("Invalid x " ++ show x)
        | q <= 0 && q == doubleFloor q = error ("Invalid q " ++ show q)
        | q > 1E8 = (1/(x - 1) + 1/(2*q)) * q**(1 - x)
        | otherwise = mainLoop (q**(-x)) q 0 0.0
    where
        secondLoop s a i b w k
            | abs (t'/s) < machep = s
            | i == 12 = s
            | otherwise = secondLoop s' (a'*(x + k')) (i + 1) (b'/w) w (k'+1)
            where
                a' = a*(x + k)
                b' = b/w
                t' = a'*b'/(coefA!!i)
                s' = s + t'
                k' = k + 1
        mainLoop s a i b
            | i < 9 || a <= 9 = mainLoop (s + b') a' (i + 1) b'
            | abs (b/s) < machep = s
            | otherwise = secondLoop (s + b*a/(x - 1) - 0.5*b) 1 0 b a 0
            where
                a' = a + 1
                b' = a'**(-x)

machep :: Double
machep = 1.11022302462515654042E-16

coefA :: [Double]
coefA = [12.0,
         -720.0,
         30240.0,
         -1209600.0,
         47900160.0,
         -1.8924375803183791606e9,
         7.47242496e10,
         -2.950130727918164224e12,
         1.1646782814350067249e14,
         -4.5979787224074726105e15,
         1.8152105401943546773e17,
         -7.1661652561756670113e18]

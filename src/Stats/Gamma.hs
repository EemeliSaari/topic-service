module Stats.Gamma
( gammaSample 
) where

import System.Random

-- k=shape, t=scale
-- Implementation based on the: https://www.hongliangjie.com/2012/12/19/how-to-generate-gamma-random-variables/
gammaSample :: Double -> Double -> StdGen -> [Double]
gammaSample k t g = [x*(main k g)**(1/k) | x <- randomRs (0, 1) g]
    where
        d = k - (1/3)
        c = 1/(sqrt (9*d))
        inner :: Double -> StdGen -> Double
        inner v g' | flag = (inner z' newG)
                   | otherwise = d*v*t
                where
                    (u, g1) = random g'
                    (z, g2) = random g1
                    (newG, _) = split g2
                    z' | z > -1/c = (1 + c*z)**3
                       | otherwise = z
                    flag = log u > (0.5*z'**2 + d - d*v + d*log v)
        main :: Double -> StdGen -> Double
        main k' g' | k' > 1 = d*(inner 0 newG)/t
                   | otherwise = main (k' + 1) newG
                where
                    (newG, _) = split g'

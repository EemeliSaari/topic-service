module Tests.Zeta
( tests 
) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Numeric.Zeta

tests :: TestTree
tests = testGroup "Zeta"
    [ testGroup "zetaf" $ 
        [ testCase "1" $ assertEqual "Test excepted output from scipy.special.zeta" (zetaf 2 1) 1.6449340668482266
        , testCase "2" $ assertEqual "Test excepted output from scipy.special.zeta" (zetaf 2 2) 0.6449340668482266
        , testCase "5" $ assertEqual "Test excepted output from scipy.special.zeta" (zetaf 2 5) 0.22132295573711533
        ]
    ]

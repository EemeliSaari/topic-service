module Tests.Gamma
( tests 
) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Numeric.Gamma

tests :: TestTree
tests = testGroup "Gamma"
    [ testGroup "polygamma" $ 
        [ testCase "2" $ assertEqual "Test excepted output from scipy.special.polygamma" (polygamma 1 2) 0.64493407
        , testCase "3" $ assertEqual "Test excepted output from scipy.special.polygamma" (polygamma 1 3) 0.39493407
        , testCase "25.5" $ assertEqual "Test excepted output from scipy.special.polygamma" (polygamma 1 25.5) 0.03999467
        ]
    , testGroup "gamma" $ 
        [ testCase "0.5" $ assertEqual "Test excepted output from scipy.special.gamma" (gammaf 0.5) 1.77245385
        , testCase "1" $ assertEqual "Test excepted output from scipy.special.gamma" (gammaf 1) 1
        , testCase "5" $ assertEqual "Test excepted output from scipy.special.gamma" (gammaf 5) 24
        ]
    ]

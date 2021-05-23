module Tests.Solve
( tests 
) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Numeric.Solve

tests :: TestTree
tests = testGroup "Solve"
    [ testGroup "polyEval" $ 
        [ testCase "0.5" $ assertEqual "Evaluate polynomial" (polyEval 0.5 [0.123, 1.269, 2.512] 1) 3.17725
        , testCase "1" $ assertEqual "Evaluate polynomial" (polyEval 1 [0.123, 1.269, 2.512] 1) 3.904
        , testCase "2.5" $ assertEqual "Evaluate polynomial" (polyEval 2.5 [0.123, 1.269, 2.512] 1) 6.453249999999999
        , testCase "5" $ assertEqual "Evaluate polynomial" (polyEval 5 [3, 0, 1] 1) 76
        ]
    ]

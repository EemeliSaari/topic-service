module Tests.Psi
( tests 
) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Numeric.Psi

tests :: TestTree
tests = testGroup "Psi"
    [ testGroup "psi" $ 
        [ testCase "1" $ assertEqual "Test excepted output from scipy.special.psi" (-0.5772156649015329) (psi 1)
        , testCase "0.25" $ assertEqual "Test excepted output from scipy.special.psi" (-4.2274535333762655) (psi 0.25)
        , testCase "1.5" $ assertEqual "Test excepted output from scipy.special.psi" 0.03648997397857652 (psi 1.5)
        , testCase "2" $ assertEqual "Test excepted output from scipy.special.psi" 0.42278433509846713 (psi 2)
        , testCase "5" $ assertEqual "Test excepted output from scipy.special.psi" 1.5061176684318003 (psi 5)
        , testCase "12" $ assertEqual "Test excepted output from scipy.special.psi" 2.4426616799758123 (psi 12)
        , testCase "-2.4" $ assertEqual "Test excepted output from scipy.special.psi" 2.090333167059191 (psi (-2.4))
        , testCase "-4.4" $ assertEqual "Test excepted output from scipy.special.psi" 2.611723541390737 (psi (-4.4))
        , testCase "25.12" $ assertEqual "Test excepted output from scipy.special.psi" 3.2036278414234034 (psi 25.12)
        , testCase "-25.12" $ assertEqual "Test excepted output from scipy.special.psi" 11.178194047274301 (psi (-25.12))
        ]
    , testGroup "dirichletExpectation" $ 
        [ testCase "[0.1234, 0.552, 1, 1.25]" $ assertEqual "" (dirichletExpectation [0.1234, 0.552, 1, 1.25]) [-9.38732865, -2.62046806, -1.47010067, -1.12033854]
        ]
    ]

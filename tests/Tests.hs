import Test.Tasty (defaultMain, testGroup)

import qualified Tests.Gamma
import qualified Tests.Zeta
import qualified Tests.Psi
import qualified Tests.Solve

main :: IO ()
main = defaultMain $ testGroup "topics"
  [ Tests.Gamma.tests
  , Tests.Zeta.tests
  , Tests.Psi.tests
  , Tests.Solve.tests
    --Tests.LDA.tests
    --Tests.Vocab.tests
  ]

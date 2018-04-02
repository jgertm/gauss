import           Dev
import           Examples

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck
import qualified Universum.Unsafe      as Unsafe


main :: IO ()
main = defaultMain tests

tests, units, props :: TestTree

tests = testGroup "Tests" [units, props]

units = testGroup "Unit tests"
  [ testCase "left unit elimination" $
      reduce foo @?= fooR
  , testCase "double negation" $
      reduce bar @?= barR
  , testCase "value plus negation yields unit" $
      reduce baz @?= bazR
  , testCase "value times inverse yields unit" $
      reduce quux @?= quuxR
  , testCase "fuzz" $
      reduce fuzz @?= fuzzR
  , testCase "constant application" $
      reduce muk @?= mukR
  , testCase "freedom of association" $
      reduce flub @?= flubR
  ]

props = testGroup "Property tests"
  [ testProperty "left unit elimination" $
      \n ->
        let n' = Constant n
            unit = Unsafe.fromJust $ leftUnit =<< structureByOp Addition
         in (reduce $ Application Addition [unit, n']) == n'
  ]

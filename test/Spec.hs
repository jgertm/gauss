{-# LANGUAGE ScopedTypeVariables #-}

import           Dev

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck
import qualified Universum.Unsafe      as Unsafe


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [units, props]

units = testGroup "Unit tests"
  [ testCase "left unit elimination" $
      reduce (1 + 0) @?= 1
  , testCase "double negation" $
      reduce (-(-1)) @?= 1
  , testCase "value plus negation yields unit" $
      reduce (1 + (-1)) @?= 0
  , testCase "value times inverse yields unit" $
      reduce (2 * (1/2)) @?= 1
  , testCase "fuzz" $
      reduce (2 * (1 * (1/2))) @?= 1
  ]

props = testGroup "Property tests"
  [ testProperty "left unit elimination" $
      \n ->
        let n' = Constant n
            Group{unit} = Unsafe.fromJust $ getGroupByOperation Addition
         in (reduce $ Application Addition [unit, n']) == n'
  ]

-- foo, baz, bar, quux, fuzz :: Expression
-- foo = 1 + 0
-- baz = (-(-1))
-- bar = 1 + (-1)
-- quux = 2 * (1/2)
-- fuzz = 2 * (1 * (1/2))

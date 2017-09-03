{-# LANGUAGE ScopedTypeVariables #-}

import           Gauss

import           Test.Tasty
import           Test.Tasty.SmallCheck


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "Addition" $
     \(x :: Integer) (y :: Integer) -> (reduce $ plus x y) == (Just $ x + y)
  , testProperty "Multiplication" $
     \(x :: Integer) (y :: Integer) -> (reduce $ times x y) == (Just $ x * y) ]

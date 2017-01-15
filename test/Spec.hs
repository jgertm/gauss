{-# LANGUAGE ScopedTypeVariables #-}

import Gauss

import Test.Tasty
import Test.Tasty.SmallCheck


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "Addition" $
     \(x :: Integer) (y :: Integer) -> (evaluate (mkApp Addition (mkLit x) (mkLit y))) == (Just $ x + y)
  , testProperty "Multiplication" $
     \(x :: Integer) (y :: Integer) -> (evaluate (mkApp Multiplication (mkLit x) (mkLit y))) == (Just $ x * y) ]

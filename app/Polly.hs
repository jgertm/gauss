module Main where

import           Dev
import           Examples
import           Universum hiding (reduce)

main :: IO ()
main = print $ reduce polly

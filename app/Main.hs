{-# LANGUAGE GADTs #-}

module Main where

import Lib

main :: IO ()
main = undefined

exOne :: Con Integer
exOne = Con (3 :: Integer)

exTwo :: App Multiplication (Tuple (Con Integer)) Integer
exTwo = App Multiplication (exOne,exOne)

exThree :: Con Double
exThree = Con 3.2

exFour :: App Multiplication (Tuple (Con Double)) Double
exFour = App Multiplication (exThree,exThree)

fnOne :: Integer -> App Multiplication (Tuple (Con Integer)) Integer
fnOne x = App Multiplication (exOne, Con x)

exFive :: App Multiplication (Con Integer, App Addition (Con Integer, Con Integer) Integer) Integer
exFive = App Multiplication (Con 2, App Addition (Con 3, Con 1))

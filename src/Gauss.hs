{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Gauss where

import           Gauss.Expressions
import           Gauss.Operations

import           ClassyPrelude

import           Data.Dynamic


example01 = Literal (toDyn (3 :: Int))

eval01 :: Maybe Int
eval01 = evaluate example01

example02 = Application Addition [example01, example01]

eval02 :: Maybe Int
eval02 = evaluate example02

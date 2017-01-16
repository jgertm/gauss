{-# LANGUAGE ScopedTypeVariables #-}

module Gauss.Reduction where

import Gauss.Expressions
import Gauss.Operations

import Data.Proxy

reduce :: Expression e -> Maybe e
reduce (Literal n) = Just n
reduce (Symbol _ _) = Nothing
reduce (Application (_ :: opT) ev) = fmap (evaluate (Proxy :: Proxy opT)) . sequence . fmap reduce $ ev

{-# LANGUAGE NoImplicitPrelude #-}

module Gauss.Expressions where

import           Gauss.Operations
import           Gauss.Types

import           ClassyPrelude

import           Data.Dynamic


type Identifier = Int

data Expression = Symbol Identifier
                | Literal Dynamic
                | Application Operation [Expression]
                deriving (Show)

evaluate :: (Typeable a)
         => Expression -> Maybe a
evaluate (Symbol _)           = Nothing
evaluate (Literal l)          = fromDynamic l
evaluate (Application op exs) = case all isLiteral exs of
  True  -> undefined
  False -> undefined

isLiteral :: Expression -> Bool
isLiteral (Literal _) = True
isLiteral _           = False

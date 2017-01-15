{-# LANGUAGE ScopedTypeVariables #-}
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

mkLit :: Typeable a => a -> Expression
mkLit n = Literal $ toDyn n
mkSym :: Identifier -> Expression
mkSym s = Symbol s
mkApp :: Operation -> Expression -> Expression -> Expression
mkApp op exL exR = Application op [exL, exR]

evaluate :: (Typeable a)
         => Expression -> Maybe a
evaluate (Symbol _)           = Nothing
evaluate (Literal l)          = fromDynamic l
evaluate (Application op exs) = do
  guard $ and [ all isLiteral exs
              , length exs > 0 ]

  domainType :: TypeRep <- fmap dynTypeRep $ fromLiteral =<< headMay exs
  fn <- lookup domainType =<< lookup op operationsTypes
  dyns <- fromNullable =<< (sequenceA . fmap fromLiteral $ exs)
  fromDynamic $ ofoldl' dynApp fn dyns

isLiteral :: Expression -> Bool
isLiteral (Literal _) = True
isLiteral _           = False

fromLiteral :: Expression -> Maybe Dynamic
fromLiteral (Literal l) = Just l
fromLiteral _           = Nothing

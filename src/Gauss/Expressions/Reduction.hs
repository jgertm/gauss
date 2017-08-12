{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gauss.Expressions.Reduction where

import           ClassyPrelude

import           Data.Dynamic


data Expression where
  Literal :: Dynamic
          -> Expression

  Symbol :: String
         -> Expression

  Application :: forall op.
                 op
              -> Seq Expression
              -> Expression

-- deriving instance Show Expression

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Gauss.Operations
       ( Operation(..), Lift

       , Addition(..)
       , Multiplication(..)

       , Monoid, Ring
       ) where

import           ClassyPrelude                   (Eq (..))

import           Gauss.Operations.Class

import           Gauss.Operations.Addition
import           Gauss.Operations.Multiplication


type Monoid (op :: *) (t :: *) = (Eq t, Operation op (t,t), Codomain op (t,t) ~ t)

type Ring (t :: *) = (Monoid Addition t, Monoid Multiplication t)

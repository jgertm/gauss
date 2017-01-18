{-# LANGUAGE TypeFamilies #-}

module Gauss.Operations.Class
       ( module Gauss.Operations.Class
       , module Data.Vector.Fixed
       ) where

import           Data.Vector.Fixed hiding (Arity, foldMap)
import           Data.Vector.Fixed.Boxed
import           GHC.Exts                (Constraint)

class (Show op) => Operation op where
  type Arity op    :: *
  type Domain op   :: * -> * -> Constraint

class (Operation op) => Eval op where
  evaluate :: (Arity op ~ n, Domain op dom cod)
           => pr op -> Vec n dom -> cod

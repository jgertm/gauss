{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

module Gauss.Types where

import           Gauss.Operations
import qualified Gauss.Types.Int       as Int
import qualified Gauss.Types.Integer   as Integer

import           ClassyPrelude

import           Data.Bifunctor.Flip
import           Data.Dynamic


type TypeMap = Map TypeRep OperationMap

typesOperations :: TypeMap
typesOperations = mapFromList $
  [ (Int.rep, Int.operations)
  , (Integer.rep, Integer.operations) ]

operationsTypes :: Map Operation (Map TypeRep Dynamic)
operationsTypes = invert2 typesOperations

invert2 :: (Ord a, Ord b)
       => Map a (Map b c) -> Map b (Map a c)
invert2 = mapFromList
       . fmap (fmap mapFromList)
       . maybe mzero id
       . sequenceA
       . fmap ( fmap runFlip . sequenceA . Flip
              . first ( unsingleton . setFromList )
              . unzip )
       . groupAllOn (\(x,_) -> x)
       . concatMap (\(a, bcs) -> fmap (\(b, c) -> (b, (a, c))) bcs)
       . mapToList
       . fmap mapToList
  where unsingleton :: Set a -> Maybe a
        unsingleton c = case length c of
          1 -> headMay c
          _ -> Nothing

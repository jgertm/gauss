{-# LANGUAGE TypeFamilyDependencies #-}

module Gauss.Argument where


newtype Unit a = Unit a deriving (Show)

class Argument args where
  type family Lift (f :: * -> *) args = lifted | lifted -> args
  amap :: (forall a. a -> a) -> args -> args

instance Argument (Unit a) where
  type Lift f (Unit a) = Unit (f a)
  amap f (Unit a) = Unit $ f a
instance Argument (a,b) where
  type Lift f (a,b) = (f a, f b)
  amap f (a,b) = (f a, f b)
instance Argument (a,b,c) where
  type Lift f (a,b,c) = (f a, f b, f c)
  amap f (a,b,c) = (f a, f b, f c)

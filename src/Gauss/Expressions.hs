module Gauss.Expressions where

import           Gauss.Operations

import           ClassyPrelude


type Identifier = Int

data Expression dom where
  Literal :: (Show dom)
          => dom
          -> Expression dom

  Symbol  :: String
          -> Identifier
          -> Expression dom

  Application :: ( Operation op
                 , Domain op args cod
                 -- , Lower Expression liftedArgs ~ args
                 )
              => op
              -> Lift Expression args
              -> Expression cod

-- instance (Show dom) => Show (Expression dom) where
--   show (Literal l)         = "(L " <> show l <> ")"
--   show (Symbol s i)        = "(S " <> s <> "_" <> show i <> ")"
--   show (Application op ev) = "(A " <> name op <> " " <> (foldMap show ev) <> ")"

-- plus :: (Additive dom cod) => dom -> dom -> Expression cod
-- plus x y = Application Addition $ ((Literal x), (Literal y))

-- times :: (Multiplicative dom cod) => dom -> dom -> Expression cod
-- times x y = Application Multiplication $ ((Literal x), (Literal y))

foo = Application Addition ((Literal (1::Int)),(Literal (2::Int)))

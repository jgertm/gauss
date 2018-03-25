{-# LANGUAGE DataKinds #-}

module Dev where

import           Control.Monad
import           Data.Vector   (Vector)


type Scalar = Double

newtype Tensor (dims :: [Int]) = Tensor (Vector Scalar)

data Expression = Variable String
                | Constant Scalar
                | Application Operation [Expression]
                deriving (Show, Eq)

getOp :: Expression -> Maybe Operation
getOp (Application op _) = Just op
getOp (Variable _)       = Nothing
getOp (Constant _)       = Nothing

instance Num Expression where
  x + y = Application Addition [x,y]
  x * y = Application Multiplication [x,y]
  x - y = Application Subtraction [x,y]
  negate x = 0 - x
  abs = undefined
  signum = undefined
  fromInteger = Constant . fromInteger

instance Fractional Expression where
  x / y = Application Division [x,y]
  recip x = 1 / x
  fromRational = Constant . fromRational

data Operation = Addition
               | Multiplication
               | Subtraction
               | Division
               | Exponentiation
               | Log
               deriving (Show, Eq)

isGroup :: Operation -> Bool
isGroup op = op `elem` [Addition, Multiplication, Exponentiation]

getGroup :: Operation -> Maybe Group
getGroup Addition = Just $ Group { operation = Addition, inverse = Subtraction, unit = 0 }
getGroup Multiplication = Just $ Group { operation = Multiplication, inverse = Division, unit = 0 }
getGroup _ = Nothing

data Group = Group { operation :: Operation
                   , inverse   :: Operation
                   , unit      :: Scalar
                   }

foo :: Expression
foo = 1 + 2

baz :: Expression
baz = (-(-1))

bar :: Expression
bar = 1 + (-1)

quux :: Expression
quux = 2 * (1/2)

type RewriteRule = Expression -> Maybe Expression

removeLeftUnitApplication, removeDoubleInverse, reduceInverseApplication :: RewriteRule

removeLeftUnitApplication (Application op [unit', val]) = do
  Group{inverse, unit} <- getGroup op
  guard $ unit == unit'
  undefined

removeDoubleInverse (Application negO [unitO, Application negI [unitI, val]]) = undefined
-- removeDoubleInverse expr | maybe False isGroup . getOp $ expr = undefined
--                          | otherwise = Nothing

reduceInverseApplication expr = undefined

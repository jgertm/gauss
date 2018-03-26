{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dev where

import           Universum   hiding (reduce)

import           Data.Vector (Vector)


type Scalar = Double

newtype Tensor (dims :: [Int]) = Tensor (Vector Scalar)

data Expression = Variable String
                | Constant Scalar
                | Application Operation [Expression]
                deriving (Show, Eq)

op :: Expression -> Maybe Operation
op (Application op _) = Just op
op _                  = Nothing

args :: Expression -> Maybe [Expression]
args (Application _ args) = Just args
args _                    = Nothing


instance Num Expression where
  x + y = Application Addition [x,y]
  x * y = Application Multiplication [x,y]
  x - y = Application Addition [x, Application Negation [y]]
  negate x = Application Negation [x]
  abs = undefined
  signum = undefined
  fromInteger = Constant . fromInteger

instance Fractional Expression where
  x / y = Application Multiplication [x, Application Inversion [y]]
  recip x = Application Inversion [x]
  fromRational = Constant . fromRational

data Operation = Addition
               | Multiplication
               | Negation
               | Inversion
               | Exponentiation
               | Log
               deriving (Show, Eq)

isGroup :: Operation -> Bool
isGroup op = op `elem` map operation groups

getGroupByOperation, getGroupByInverse :: Operation -> Maybe Group
getGroupByOperation op = find (\g -> op == operation g) groups
getGroupByInverse iop = find (\g -> iop == inverse g) groups

groups :: [Group]
groups =
  [ Group { operation = Addition, inverse = Negation, unit = 0 }
  , Group { operation = Multiplication, inverse = Inversion, unit = 1 }
  ]

data Group = Group { operation :: Operation
                   , inverse   :: Operation
                   , unit      :: Expression
                   }

foo, baz, bar, quux :: Expression
foo = 1 + 0
baz = (-(-1))
bar = 1 + (-1)
quux = 2 * (1/2)

type RewriteRule = Expression -> Maybe Expression

doNothing, removeLeftUnitApplication, removeRightUnitApplication, removeUnitApplication, removeDoubleInverse, removeInverseApplication :: RewriteRule

doNothing = Just

removeLeftUnitApplication (Application op [unit', val]) = do
  Group{unit} <- getGroupByOperation op
  guard $ unit == unit'
  pure val
removeLeftUnitApplication _ = Nothing

removeRightUnitApplication (Application op [val, unit']) = do
  Group{unit} <- getGroupByOperation op
  guard $ unit == unit'
  pure val
removeRightUnitApplication _ = Nothing

removeUnitApplication = liftA2 (<|>) removeLeftUnitApplication removeRightUnitApplication

removeDoubleInverse (Application negO [Application negI [val]]) = do
  guard $ negO == negI
  Group{} <- getGroupByInverse negO
  pure val
removeDoubleInverse _ = Nothing

removeInverseApplication (Application op [x,y]) = do
  Group{unit, inverse} <- getGroupByOperation op
  let xinv = Application inverse [x]
  xinv' <- removeDoubleInverse xinv <|> pure xinv
  guard $ xinv' == y
  pure unit
removeInverseApplication _ = Nothing

rules :: [RewriteRule]
rules = [ doNothing
        , removeLeftUnitApplication
        , removeRightUnitApplication
        , removeDoubleInverse
        , removeInverseApplication
        ]

-- quux = Application Multiplication [Constant 2.0,Application Multiplication [Constant 1.0,Application Inversion [Constant 2.0]]]

-- reduce :: Expression -> Expression
reduce (Application op args) = catMaybes $ do
  args' <- sequence $ map (\a -> catMaybes $ map ($ a) rules) args
  let expr = Application op args'
  rule <- rules
  pure $ rule expr
reduce expr = [expr]

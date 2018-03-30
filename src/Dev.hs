{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}

module Dev where

import           Universum    hiding (reduce, show)

import           Data.Vector  (Vector)
import           GHC.TypeLits
import           Text.Printf
import           Text.Show


type Scalar = Double

newtype Tensor (dims :: [Int]) = Tensor (Vector Scalar)

data Expression = Variable String
                | Constant Scalar
                | Application Operation [Expression]
                deriving (Eq)

instance Show Expression where
  show (Variable v) = v
  show (Constant c) = show c
  show (Application op args) =
    let opSym = show op
        inner = case operatorFixity op of
          Prefix  -> opSym <> " " <> (intercalate " " $ map show args)
          Infix   -> intercalate opSym $ map show args
          Postfix -> (intercalate " " $ map show args) <> opSym
     in printf "(%s)" inner

instance (KnownSymbol symbol) => IsLabel symbol Expression where
  fromLabel = Variable (symbolVal p)
    where p :: Proxy symbol
          p = Proxy

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
               deriving (Eq)

instance Show Operation where
  show op = case op of
              Addition       -> "+"
              Multiplication -> " "
              Negation       -> "−"
              Inversion      -> "^(-1)"
              Exponentiation -> "^"
              Log            -> "log"

data Fixity = Prefix
            | Infix
            | Postfix
            deriving (Show, Eq)

operatorFixity :: Operation -> Fixity
operatorFixity op
  | op `elem` [Addition, Multiplication, Exponentiation] = Infix
  | op `elem` [Negation, Log] = Prefix
  | op `elem` [Inversion] = Postfix

isGroup :: Operation -> Bool
isGroup op = op `elem` map operation groups

getGroupByOperation, getGroupByInverse :: Operation -> Maybe Group
getGroupByOperation op = find (\g -> op == operation g) groups
getGroupByInverse iop = find (\g -> iop == inverse g) groups

groups :: [Group]
groups =
  [ Group { operation = Addition, inverse = Negation, unit = 0, properties = [Commutative] }
  , Group { operation = Multiplication, inverse = Inversion, unit = 1, properties = [Commutative, Distributive Addition] }
  ]

data Property = Commutative
              | Distributive Operation
              deriving (Show, Eq)

data Group = Group { operation  :: Operation
                   , inverse    :: Operation
                   , unit       :: Expression
                   , properties :: [Property]
                   } deriving (Show, Eq)

type RewriteRule = Expression -> [Expression]

doNothing, removeLeftUnitApplication, removeRightUnitApplication, removeUnitApplication, removeDoubleInverse, removeInverseApplication, commuteArguments :: RewriteRule

doNothing = pure

removeLeftUnitApplication (Application op [unit', val]) = do
  Group{unit} <- getGroupByOperation op
  guard $ unit == unit'
  pure val
removeLeftUnitApplication _ = fail "rule not applicable"

removeRightUnitApplication (Application op [val, unit']) = do
  Group{unit} <- getGroupByOperation op
  guard $ unit == unit'
  pure val
removeRightUnitApplication _ = fail "rule not applicable"

removeUnitApplication = liftA2 (<|>) removeLeftUnitApplication removeRightUnitApplication

removeDoubleInverse (Application negO [Application negI [val]]) = do
  guard $ negO == negI
  Group{} <- getGroupByInverse negO
  pure val
removeDoubleInverse _ = fail "rule not applicable"

removeInverseApplication (Application op [x,y]) = do
  Group{unit, inverse} <- getGroupByOperation op
  let xinv = Application inverse [x]
  xinv' <- removeDoubleInverse xinv <|> pure xinv
  guard $ xinv' == y
  pure unit
removeInverseApplication _ = fail "rule not applicable"

commuteArguments (Application op args) = do
  Group{properties} <- getGroupByOperation op
  guard $ Commutative `elem` properties
  args' <- permutations args
  pure $ Application op args'
commuteArguments _ = fail "rule not applicable"

rules :: [RewriteRule]
rules = [ doNothing
        , removeLeftUnitApplication
        , removeRightUnitApplication
        , removeDoubleInverse
        , removeInverseApplication
        , commuteArguments
        ]

reductions :: Expression -> [Expression]
reductions (Application op args) = do
  args' <- traverse reductions args
  let expr = Application op args'
  rule <- rules
  rule expr
reductions expr = [expr]

nodes :: Expression -> [Expression]
nodes c@(Constant _)         = [c]
nodes v@(Variable _)         = [v]
nodes a@(Application _ args) = a : concatMap nodes args

score :: Expression -> Int
score (Constant _)         = 1
score (Variable _)         = 2
score (Application _ args) = 5 + (sum . map score $ args)

reduce :: Expression -> Expression
reduce = minimumBy (compare `on` score) . reductions

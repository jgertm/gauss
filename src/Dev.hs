{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}

module Dev where

import           Universum    hiding (Either (..), reduce, show)

import qualified Data.HashSet as HS
import           Data.Vector  (Vector)
import           GHC.TypeLits
import           Text.Printf
import           Text.Show


type Scalar = Double

newtype Tensor (dims :: [Int]) = Tensor (Vector Scalar)

data Expression = Variable String
                | Constant Scalar
                | Application Operation [Expression]
                deriving (Eq, Generic)

instance Hashable Expression

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
  fromInteger = Constant . fromInteger

instance Fractional Expression where
  x / y = Application Multiplication [x, Application Inversion [y]]
  recip x = Application Inversion [x]
  fromRational = Constant . fromRational

isConstant :: Expression -> Bool
isConstant (Constant _) = True
isConstant _            = False

value :: Expression -> Maybe Scalar
value (Constant c) = Just c
value _            = Nothing

data Operation = Addition
               | Multiplication
               | Negation
               | Inversion
               | Exponentiation
               | Log
               deriving (Eq, Generic)

instance Hashable Operation

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
            deriving (Show, Eq, Generic)

operatorFixity :: Operation -> Fixity
operatorFixity op
  | op `elem` [Addition, Multiplication, Exponentiation] = Infix
  | op `elem` [Negation, Log] = Prefix
  | op `elem` [Inversion] = Postfix
  | otherwise = error . toText $ "Undefined fixity for operation: " <> show op

isGroup :: Operation -> Bool
isGroup op = op `elem` map operation groups

getGroupByOperation, getGroupByInverse :: (MonadFail m) => Operation -> m Group
getGroupByOperation op = maybe (fail "group not found") pure $ find (\g -> op == operation g) groups
getGroupByInverse iop = maybe (fail "group not found") pure $ find (\g -> iop == inverse g) groups

groups :: [Group]
groups =
  [ Group { operation = Addition, inverse = Negation, unit = 0, properties = [Associative Left, Associative Right, Commutative] }
  , Group { operation = Multiplication, inverse = Inversion, unit = 1, properties = [Associative Left, Associative Right, Commutative, Distributive Addition] }
  ]

compute :: Operation -> [Scalar] -> Scalar
compute Addition       = sum
compute Multiplication = product
compute Negation       = maybe 0 (negate . head) . nonEmpty
compute Inversion      = maybe 1 (recip . head) . nonEmpty
compute Exponentiation = error "Undefined operation"
compute Log            = error "Undefined operation"

data Property = Associative Side
              | Commutative
              | Distributive Operation
              deriving (Show, Eq)

data Side = Left
          | Right
          deriving (Show, Eq)

data Group = Group { operation  :: Operation
                   , inverse    :: Operation
                   , unit       :: Expression
                   , properties :: [Property]
                   } deriving (Show, Eq)

type RewriteRule = Expression -> [Expression]

doNothing, removeLeftUnitApplication, removeRightUnitApplication, removeUnitApplication, removeDoubleInverse, removeInverseApplication, commuteArguments, applyOperation, reassociateLeft, reassociateRight :: RewriteRule

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

applyOperation (Application op args) = do
  guard $ all isConstant args
  pure . Constant . compute op $ catMaybes $ map value $ args
applyOperation _                     = fail "rule not applicable"

reassociateLeft (Application opO [Application opI [x,y], z]) = do
  guard $ opO == opI
  Group{properties} <- getGroupByOperation opO
  guard $ Associative Left `elem` properties
  pure $ Application opO [x, Application opI [y,z]]
reassociateLeft _ = fail "rule not applicable"

reassociateRight (Application opO [x, Application opI [y,z]]) = do
  guard $ opO == opI
  Group{properties} <- getGroupByOperation opO
  guard $ Associative Right `elem` properties
  pure $ Application opO [Application opI [x,y], z]
reassociateRight _ = fail "rule not applicable"

rules :: [RewriteRule]
rules = [ doNothing
        , removeLeftUnitApplication
        , removeRightUnitApplication
        , removeDoubleInverse
        , removeInverseApplication
        , commuteArguments
        , applyOperation
        , reassociateLeft
        , reassociateRight
        ]

reductions :: Expression -> HashSet Expression
reductions expr = executingState mempty $ go expr
  where go :: Expression -> State (HashSet Expression) ()
        go (Application op args) = do
          seenRewrites <- get
          let newRewrites :: HashSet Expression = HS.fromList $ do
                args' <- traverse (HS.toList . reductions) args
                rule <- rules
                result <- rule $ Application op args'
                pure result
              frontier = newRewrites `HS.difference` seenRewrites
          modify $ HS.union newRewrites
          traverse_ go frontier
        go expr = put $ one expr

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

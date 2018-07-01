{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dev
  ( Expression(..)
  , reduce
  , reductions
  , rules
  , focus
  , foci
  , index
  , replace
  , nodes
  , score
  ) where

import           Universum           hiding (Either (..), Identity, reduce)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Set            as S
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           GHC.TypeLits
import           Text.Printf
import qualified Text.Show           as TS

type Scalar = Double

data Expression
  = Variable String
  | Constant Scalar
  | Application Operation
                [Expression]
  deriving (Eq, Ord, Generic)

instance Hashable Expression

instance Show Expression where
  show (Variable v) = v
  show (Constant c) =
    if fromIntegral (truncate c) == c
      then show $ truncate c
      else show c
  show (Application op args) =
    let opSym = show op
        inner =
          case operatorFixity op of
            Prefix  -> unwords $ opSym : map show args
            Infix   -> mconcat $ intersperse opSym $ map show args
            Postfix -> unwords (map show args) <> opSym
     in printf "(%s)" inner

instance (KnownSymbol symbol) => IsLabel symbol Expression where
  fromLabel = Variable (symbolVal p)
    where
      p :: Proxy symbol
      p = Proxy

instance Num Expression where
  x + y = Application Addition [x, y]
  x * y = Application Multiplication [x, y]
  x - y = Application Addition [x, Application Negation [y]]
  negate x = Application Negation [x]
  fromInteger = Constant . fromInteger

instance Fractional Expression where
  x / y = Application Multiplication [x, recip y]
  recip x = Application Inversion [x]
  fromRational = Constant . fromRational

instance Floating Expression where
  x ** y = Application Exponentiation [x, y]

cataM :: (Monad m) => (Expression -> m Expression) -> Expression -> m Expression
cataM f (Application op args) = do
  args' <- traverse f args
  f (Application op args')
cataM f expr = f expr

op :: Expression -> Maybe Operation
op (Application op _) = Just op
op _                  = Nothing

args :: Expression -> Maybe [Expression]
args (Application _ args) = Just args
args _                    = Nothing

isConstant :: Expression -> Bool
isConstant (Constant _) = True
isConstant _            = False

value :: Expression -> Maybe Scalar
value (Constant c) = Just c
value _            = Nothing

data Operation
  = Addition
  | Multiplication
  | Negation
  | Inversion
  | Exponentiation
  | Ln
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Hashable Operation

instance Show Operation where
  show =
    \case
      Addition -> "+"
      Multiplication -> " "
      Negation -> "-"
      Inversion -> "^(-1)"
      Exponentiation -> "^"
      Ln -> "ln"

data Fixity
  = Prefix
  | Infix
  | Postfix
  deriving (Show, Eq, Generic)

operatorFixity :: Operation -> Fixity
operatorFixity op
  | op `elem` [Addition, Multiplication, Exponentiation] = Infix
  | op `elem` [Negation, Ln] = Prefix
  | op == Inversion = Postfix
  | otherwise =
    error . toText $ "Undefined fixity for operation: " <> TS.show op

data Property
  = Associative { getSide :: Side }
  | Commutative
  | Distributive { getSide      :: Side
                 , getOperation :: Operation }
  | Closure
  | Identity { getSide :: Side
             , getUnit :: Expression }
  | Inverse { getOperation :: Operation }
  deriving (Eq, Generic)

instance Hashable Property

data Side
  = Left
  | Right
  deriving (Show, Eq, Generic)

instance Hashable Side

data Structure = Structure
  { operation  :: Operation
  , properties :: HashSet Property
  }

instance Eq Structure where
  (Structure a _) == (Structure b _) = a == b

data Structures = Structures
  { byOp          :: Vector (Maybe Structure)
  , byInverse     :: Vector (Maybe Structure)
  , byDistributor :: Vector (Maybe Structure)
  }

structures :: Structures
structures =
  Structures
    { byOp = map (\op -> find (\struct -> operation struct == op) structL) opV
    , byInverse =
        map
          (\op -> find (\(Structure _ props) -> Inverse op `elem` props) structL)
          opV
    , byDistributor =
        map
          (\op ->
             find
               (\(Structure _ props) ->
                  (Distributive Left op `elem` props) ||
                  (Distributive Right op `elem` props))
               structL)
          opV
    }
  where
    opV = V.enumFromTo minBound maxBound
    structL =
      [ Structure Addition $
        HS.fromList
          [ Associative Left
          , Associative Right
          , Commutative
          , Closure
          , Identity Left 0
          , Identity Right 0
          , Inverse Negation
          ]
      , Structure Multiplication $
        HS.fromList
          [ Associative Left
          , Associative Right
          , Commutative
          , Distributive Left Addition
          , Distributive Right Addition
          , Closure
          , Identity Left 1
          , Identity Right 1
          , Inverse Inversion
          ]
      , Structure Exponentiation $
        HS.fromList
          [ Distributive Right Multiplication
          , Closure
          , Identity Right 1
          , Inverse Ln
          ]
      ]

structureByOp, structureByInverse, structureByDistributor ::
     Operation -> Maybe Structure
structureByOp op = V.unsafeIndex (byOp structures) (fromEnum op)

structureByInverse inv = V.unsafeIndex (byInverse structures) (fromEnum inv)

structureByDistributor op =
  V.unsafeIndex (byDistributor structures) (fromEnum op)

leftUnit, rightUnit :: Structure -> Maybe Expression
leftUnit =
  map getUnit .
  find
    (\case
       Identity Left _ -> True
       _ -> False) .
  properties

rightUnit =
  map getUnit .
  find
    (\case
       Identity Right _ -> True
       _ -> False) .
  properties

inverse :: Structure -> Maybe Operation
inverse =
  map getOperation .
  find
    (\case
       Inverse _ -> True
       _ -> False) .
  properties

distributes :: Structure -> Maybe Operation
distributes =
  map getOperation .
  find
    (\case
       Distributive _ _ -> True
       _ -> False) .
  properties

type RewriteRule = Expression -> [Expression]

doNothing, removeLeftUnitApplication, removeRightUnitApplication, removeDoubleInverse, removeInverseApplication, commute, apply, reassociate, distribute, factor, raise, expandInverse, absorbInverse ::
     RewriteRule
doNothing = pure

removeLeftUnitApplication (Application op [unit', val]) =
  maybeToList $ do
    unit <- leftUnit =<< structureByOp op
    guard $ unit == unit'
    pure val
removeLeftUnitApplication _ = fail "rule not applicable"

removeRightUnitApplication (Application op [val, unit']) =
  maybeToList $ do
    unit <- rightUnit =<< structureByOp op
    guard $ unit == unit'
    pure val
removeRightUnitApplication _ = fail "rule not applicable"

removeDoubleInverse (Application negO [Application negI [val]]) =
  maybeToList $ do
    _ <- structureByInverse negO
    guard $ negO == negI
    pure val
removeDoubleInverse _ = fail "rule not applicable"

removeInverseApplication (Application op [x, y]) =
  maybeToList $ do
    structure <- structureByOp op
    unit <- leftUnit structure
    inverse <- inverse structure
    let xinv = Application inverse [x]
    xinv' <- safeHead $ removeDoubleInverse xinv <|> pure xinv
    guard $ xinv' == y
    pure unit
removeInverseApplication _ = fail "rule not applicable"

commute (Application op args) = do
  guard $ elem Commutative . maybeToMonoid . map properties $ structureByOp op
  args' <- filter (/= args) $ permutations args
  pure $ Application op args'
commute _ = fail "rule not applicable"

apply (Application op args) =
  maybeToList $ do
    guard $ all isConstant args
    f <-
      case op of
        Addition       -> Just sum
        Multiplication -> Just product
        _              -> Nothing
    pure . Constant . f . mapMaybe value $ args
apply _ = fail "rule not applicable"

reassociate (Application op [Application op' [x, y], z]) =
  maybeToList $ do
    guard $ op == op'
    guard $
      elem (Associative Left) . maybeToMonoid . map properties $
      structureByOp op
    pure $ Application op [x, Application op' [y, z]]
reassociate (Application op [x, Application op' [y, z]]) =
  maybeToList $ do
    guard $ op == op'
    guard $
      elem (Associative Right) . maybeToMonoid . map properties $
      structureByOp op
    pure $ Application op [Application op' [x, y], z]
reassociate _ = fail "rule not applicable"

distribute (Application opO [a, Application opI [b, c]]) =
  maybeToList $ do
    Structure _ props <- structureByOp opO
    guard $ Distributive Left opI `elem` props
    pure $ Application opI [Application opO [a, b], Application opO [a, c]]
distribute (Application opO [Application opI [b, c], a]) =
  maybeToList $ do
    Structure _ props <- structureByOp opO
    guard $ Distributive Right opI `elem` props
    pure $ Application opI [Application opO [b, a], Application opO [c, a]]
distribute _ = fail "rule not applicable"

factor (Application opO [Application opIL [a, b], Application opIR [a', b']]) = do
  guard $ opIL == opIR
  Structure _ props <- maybeToList $ structureByOp opIL
  let leftFactor = do
        guard $ a == a' && Distributive Left opO `elem` props
        pure $ Application opIL [a, Application opO [b, b']]
      rightFactor = do
        guard $ b == b' && Distributive Right opO `elem` props
        pure $ Application opIL [Application opO [a, a'], b]
  catMaybes [leftFactor, rightFactor]
factor _ = fail "rule not applicable"

raise (Application op as@(a:_)) = do
  guard $ all (== a) as
  Structure liftedOp _ <- maybeToList $ structureByDistributor op
  pure $ Application liftedOp [a, Constant . fromIntegral $ length as]
raise _ = fail "rule not applicable"

expandInverse (Application inv [x]) =
  maybeToMonoid $ do
    Structure op _ <- structureByInverse inv
    struct@(Structure op' _) <- structureByDistributor op
    let leftExpansion = do
          unit <- leftUnit struct
          guard $ x /= unit
          pure $ Application op' [Application inv [unit], x]
        rightExpansion = do
          unit <- rightUnit struct
          guard $ x /= unit
          pure $ Application op' [x, Application inv [unit]]
    pure $ catMaybes [leftExpansion, rightExpansion]
expandInverse _ = fail "rule not applicable"

absorbInverse (Application op [Application inv [unit], x]) =
  maybeToList $ do
    struct <- structureByOp op
    unit' <- leftUnit struct
    inv' <- inverse =<< structureByOp =<< distributes struct
    guard $ unit == unit' && inv == inv'
    pure $ Application inv [x]
absorbInverse (Application op [x, Application inv [unit]]) =
  maybeToList $ do
    struct <- structureByOp op
    unit' <- rightUnit struct
    guard $ unit == unit'
    inv' <- inverse =<< structureByOp =<< distributes struct
    guard $ inv == inv'
    pure $ Application inv [x]
absorbInverse _ = fail "rule not applicable"

rules :: [RewriteRule]
rules =
  [ doNothing
  , removeLeftUnitApplication
  , removeRightUnitApplication
  , removeDoubleInverse
  , removeInverseApplication
  -- , commute -- FIXME
  , apply
  , reassociate
  , distribute
  , factor
  , raise
  , expandInverse
  , absorbInverse
  ]

reductions :: Expression -> Set Expression
reductions expr = executingState mempty $ go expr
  where
    go :: Expression -> State (Set Expression) ()
    go (Application op args) = do
      seenRewrites <- get
      let newRewrites =
            S.fromList $ do
              args' <- traverse (S.toList . reductions) args
              rule <- rules
              rule $ Application op args'
          frontier = newRewrites `S.difference` seenRewrites
      modify $ S.union newRewrites
      traverse_ go frontier
    go expr = put $ one expr

focus :: Expression -> [Int] -> Maybe Expression
focus exp [] = Just exp
focus (Application _ exps) (i:is) = do
  exp <- exps `index` i
  focus exp is
focus _ _ = Nothing

foci :: Expression -> [[Int]]
foci (Application _ exps) = tops <> subs
  where
    tops = [[i] | i <- [0 .. pred $ length exps]]
    subs = concat . zipWith (\t ss -> map (t <>) ss) tops . map foci $ exps
foci _ = mempty

replace :: Expression -> [Int] -> Expression -> Maybe Expression
replace _ [] new = Just new
replace exp@(Application op exps) path@(i:is) new = do
  guard $ isJust (exp `focus` path)
  new' <- replace down is new
  pure $ Application op (mconcat [pre, [new'], post])
  where
    (pre, down:post) = splitAt i exps
replace _ _ _ = Nothing

index :: [a] -> Int -> Maybe a
index xs i = safeHead . snd . splitAt i $ xs

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

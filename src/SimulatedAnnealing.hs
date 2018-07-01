module SimulatedAnnealing
  () where

import           Universum
import qualified Universum.Unsafe        as Unsafe

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.STRef
import           Dev
import           System.Random.MWC

anneal :: Expression -> Expression
anneal expr =
  runST $ do
    gen <- create
    solutionR <- newSTRef expr
    tempR <- newSTRef (1 :: Float)
    let go f = do
          score' <- score <$> readSTRef solutionR
          inner *> modifySTRef' tempR (* 0.99)
          score'' <- score <$> readSTRef solutionR
          if score'' < score'
            then f
            else readSTRef solutionR
        inner = forM_ [1 .. iterations] $ \_ -> do
            temp <- readSTRef tempR
            r <- uniform gen
            solution <- readSTRef solutionR
            -- generate random transition from S to Si
            location <- sampleST gen $ foci solution
            let subexpr = Unsafe.fromJust $ focus solution location
                rewrites = concatMap (\rule -> rule subexpr) rules
            transition <- sampleST gen rewrites
            let solution' =
                  Unsafe.fromJust $ replace solution location transition
                [sc, sc'] = map score [solution, solution']
                exponential = exp $ fromIntegral (sc - sc') / (k * temp)
            -- if new solution is better or it gets lucky, replace
            when ((sc' < sc || r < exponential) && solution /= solution') $ do
              -- traceShowM solution'
              -- traceShowM (sc,sc')
              -- traceShowM (r, exponential)
              writeSTRef solutionR solution'
    fix go
  where
    iterations = 10 ^ 4
    k = 0.1

sampleST :: PrimMonad m => Gen (PrimState m) -> [a] -> m a
sampleST gen xs = do
  idx <- uniformR (0, pred $ length xs) gen
  pure $ Unsafe.fromJust $ xs `index` idx

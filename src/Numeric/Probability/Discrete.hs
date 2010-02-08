{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Numeric.Probability.Discrete
  ( Probs(..)
  , probsIndex, probsRandom, probsLogs
  , makeProbsFast, makeProbsLean
  ) where

import Control.Monad (liftM)
import Control.Monad.Random.Class (MonadRandom(getRandom))
import Data.Array.IArray (IArray, Array, (!), listArray)
import Data.Array.Unboxed (UArray)
import Data.Maybe (fromJust)
import Numeric.Search.Range (searchFromTo)
import System.Random (Random)

data Probs state prob =
  Probs
  { probsFunc :: state -> prob
  , probsExt :: ProbsExt state prob
  }

data ProbsExt state prob
  = ProbsLean [state]
  | ProbsFast
    { probsState :: Int -> state
    , probsLog :: Int -> prob
    , probsAccum :: Int -> prob
    , probsNumStates :: Int
    }

makeProbsLean
  :: Floating prob
  => [state] -> (state -> prob) -> Probs state prob
makeProbsLean states func =
  Probs func $ ProbsLean states

makeProbsFast
  :: forall prob state. (Floating prob, IArray UArray prob)
  => [state] -> (state -> prob) -> Probs state prob
makeProbsFast states func =
  Probs func $
  ProbsFast
  { probsNumStates = numStates
  , probsLog = (logsArr !)
  , probsState = (stateArr !)
  , probsAccum = (accumArr !)
  }
  where
    numStates = length states
    rng = (0, numStates - 1)
    stateArr :: Array Int state
    stateArr = listArray rng states
    logsArr :: UArray Int prob
    logsArr = listArray rng . map (log . func) $ states
    accumArr :: UArray Int prob
    accumArr
      = listArray rng
      . scanl (+) 0 . map func
      . init $ states

probsIndex :: Ord prob => Probs state prob -> prob -> state
probsIndex (Probs probFunc (ProbsLean states)) idx
  = fst . head
  . dropWhile ((<= idx) . snd)
  . map f $ states
  where
    f x = (x, probFunc x)
probsIndex (Probs _ pf) idx
  = probsState pf . fromJust
  $ searchFromTo ((>= idx) . probsAccum pf) 0 (probsNumStates pf - 1)


probsLogs :: Floating prob => Probs state prob -> [(state, prob)]
probsLogs (Probs probFunc (ProbsLean states)) =
  map f states
  where
    f x = (x, (log . probFunc) x)
probsLogs (Probs _ pf) =
  map f [0 .. probsNumStates pf - 1]
  where
    f i = (probsState pf i, probsLog pf i)

probsRandom
  :: (Fractional prob, Ord prob, Random prob, MonadRandom m)
  => Probs state prob -> m state
probsRandom probs = liftM (probsIndex probs) getRandom

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Numeric.Probability.Discrete
  ( Probs(..)
  , probsIndex, probsRandom, probsLogs, probsStates
  , makeProbsFast, makeProbsLean
  ) where

import Control.Monad (liftM)
import Control.Monad.Random.Class (MonadRandom(getRandom))
import Data.Array.IArray (IArray, (!), listArray)
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
    { probsStateIdx :: state -> Int
    , probsState :: Int -> state
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
  :: forall prob state
   . (Integral state, Floating prob, IArray UArray prob)
  => (state, state) -> (state -> prob) -> Probs state prob
makeProbsFast (stateStart, stateEnd) func =
  Probs func $
  ProbsFast
  { probsNumStates = numStates
  , probsState = (+ stateStart) . fromIntegral
  , probsStateIdx = fromIntegral . (+ negate stateStart)
  , probsLog = (logsArr !)
  , probsAccum = (accumArr !)
  }
  where
    numStates = fromIntegral $ stateEnd + 1 - stateStart
    states = [stateStart .. stateEnd]
    rng = (0, numStates - 1)
    logsArr :: UArray Int prob
    logsArr = listArray rng . map (log . func) $ states
    accumArr :: UArray Int prob
    accumArr
      = listArray rng
      . scanl (+) 0 . map func
      . init $ states

probsStates :: Probs state prob -> [state]
probsStates (Probs _ (ProbsLean states)) = states
probsStates (Probs _ pf) = map (probsState pf) [0 .. probsNumStates pf - 1]

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

probsLogs :: Floating prob => Probs state prob -> state -> prob
probsLogs (Probs probFunc (ProbsLean _)) = log . probFunc
probsLogs (Probs _ pf) = probsLog pf . probsStateIdx pf

probsRandom
  :: (Fractional prob, Ord prob, Random prob, MonadRandom m)
  => Probs state prob -> m state
probsRandom probs = liftM (probsIndex probs) getRandom

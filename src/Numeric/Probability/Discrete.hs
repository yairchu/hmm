{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Numeric.Probability.Discrete
  ( Probs(..)
  , probsIndex, probsRandom
  , makeProbsFastRand, makeProbsLean
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
  | ProbsFastRand
    { probsState :: Int -> state
    , probsAccum :: Int -> prob
    , probsNumStates :: Int
    }

makeProbsLean :: [state] -> (state -> prob) -> Probs state prob
makeProbsLean states func =
  Probs func (ProbsLean states)

makeProbsFastRand
  :: forall prob state. (Num prob, IArray UArray prob)
  => [state] -> (state -> prob) -> Probs state prob
makeProbsFastRand states func =
  Probs func $
  ProbsFastRand
  { probsNumStates = numStates
  , probsState = (stateArr !)
  , probsAccum = (accumArr !)
  }
  where
    numStates = length states
    stateArr :: Array Int state
    stateArr = listArray (0, numStates - 1) states
    accumArr :: UArray Int prob
    accumArr
      = listArray (0, numStates - 1)
      . scanl (+) 0 . map func
      . init $ states

probsIndex :: Ord prob => Probs state prob -> prob -> state
probsIndex (Probs _ (ProbsFastRand si acc ns)) idx
  = si . fromJust
  $ searchFromTo ((>= idx) . acc) 0 (ns - 1)
probsIndex (Probs probFunc (ProbsLean states)) idx
  = fst . head
  . dropWhile ((<= idx) . snd)
  . map f $ states
  where
    f x = (x, probFunc x)

probsRandom
  :: (Fractional prob, Ord prob, Random prob, MonadRandom m)
  => Probs state prob -> m state
probsRandom probs = liftM (probsIndex probs) getRandom


{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Numeric.Probability.Discrete
  ( Probs(..)
  , makeProbs, probsIndex, probsRandom
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
  , probsNumStates :: Int
  , probsState :: Int -> state
  , probsAccum :: Int -> prob
  }

makeProbs
  :: forall prob state. (Num prob, IArray UArray prob)
  => [state] -> (state -> prob) -> Probs state prob
makeProbs states func =
  Probs
  { probsFunc = func
  , probsNumStates = numStates
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
probsIndex probs idx
  = probsState probs
  . fromJust
  . searchFromTo ((>= idx) . probsAccum probs) 0
  $ probsNumStates probs - 1

probsRandom
  :: (Fractional prob, Ord prob, Random prob, MonadRandom m)
  => Probs state prob -> m state
probsRandom probs = liftM (probsIndex probs) getRandom


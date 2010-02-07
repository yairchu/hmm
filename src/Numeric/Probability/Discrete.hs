module Numeric.Probability.Discrete
  ( Probs(..)
  , makeProbs, probsIndex
  ) where

import Data.Array

data Probs state prob =
  Probs
  { probsFunc :: state -> prob
  , probsNumStates :: Int
  , probsState :: Int -> state
  , probsAccum :: Int -> prob
  }

makeProbs :: Num prob => [state] -> (state -> prob) -> Probs state prob
makeProbs states func =
  Probs
  { probsFunc = func
  , probsNumStates = numStates
  , probsState = (stateArr !)
  , probsAccum = (accumArr !)
  }
  where
    numStates = length states
    stateArr = listArray (0, numStates - 1) states
    accumArr
      = listArray (0, numStates - 1)
      . scanl (+) 0 . map func
      . init $ states

probsIndex :: Ord prob => Probs state prob -> prob -> state
probsIndex = undefined

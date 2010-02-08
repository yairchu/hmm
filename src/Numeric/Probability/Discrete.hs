{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Numeric.Probability.Discrete
  ( Probs(..)
  , probsIndex, probsRandom, probsLog, probsStates
  , makeProbsFast, makeProbsLean
  ) where

import Data.IndexRange

import Control.Monad (liftM)
import Control.Monad.Random.Class (MonadRandom(getRandom))
import Data.Array.IArray (IArray, (!), listArray)
import Data.Array.Unboxed (UArray)
import Data.Bijection
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
    { probsFStateRange :: IndexRange state
    , probsFLog :: state -> prob
    , probsFAccum :: Int -> prob
    }

makeProbsLean
  :: Floating prob
  => [state] -> (state -> prob) -> Probs state prob
makeProbsLean states func =
  Probs func $ ProbsLean states

makeProbsFast
  :: forall prob state
   . (Floating prob, IArray UArray prob)
  => IndexRange state -> (state -> prob) -> Probs state prob
makeProbsFast stateRange func =
  Probs func
  ProbsFast
  { probsFStateRange = stateRange
  , probsFLog = irUMemo stateRange (log . func)
  , probsFAccum = (accumArr !)
  }
  where
    states = irRange stateRange
    rng = (0, irSize stateRange - 1)
    accumArr :: UArray Int prob
    accumArr
      = listArray rng
      . scanl (+) 0 . map func
      . init $ states

probsStates :: Probs state prob -> [state]
probsStates (Probs _ (ProbsLean states)) = states
probsStates (Probs _ pf) = irRange $ probsFStateRange pf

probsIndex :: Ord prob => Probs state prob -> prob -> state
probsIndex (Probs probFunc (ProbsLean states)) idx
  = fst . head
  . dropWhile ((<= idx) . snd)
  . map f $ states
  where
    f x = (x, probFunc x)
probsIndex (Probs _ pf) idx
  = (biTo . irIndex . probsFStateRange) pf . fromJust
  . searchFromTo ((>= idx) . probsFAccum pf) 0
  $ (irSize . probsFStateRange) pf - 1

probsLog :: Floating prob => Probs state prob -> state -> prob
probsLog (Probs probFunc (ProbsLean _)) = log . probFunc
probsLog (Probs _ pf) = probsFLog pf

probsRandom
  :: (Fractional prob, Ord prob, Random prob, MonadRandom m)
  => Probs state prob -> m state
probsRandom probs = liftM (probsIndex probs) getRandom


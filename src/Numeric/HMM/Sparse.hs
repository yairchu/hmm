{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Numeric.HMM.Sparse
  ( Hmm(..), HmmCalc(..)
  , hmmCLayerProbs, hmmCalc, hmmGenerate
  , makeDenseHmm
  ) where

import Numeric.Probability.Discrete

import Control.Applicative (liftA2)
import Control.Monad (liftM)
import Control.Monad.ListT (ListT)
import Control.Monad.Random.Class (MonadRandom)
import Data.Array.IArray (IArray, Array, (!), listArray)
import Data.Array.Unboxed (UArray)
import Data.Ix (Ix, range, rangeSize)
import Data.List.Class (iterateM, toList)
import System.Random (Random)

data Hmm state obs prob =
  Hmm
  { hmmStartProbs :: Probs state prob
  , hmmTransitionProbs :: state -> Probs state prob
  , hmmObservationProbs :: state -> Probs obs prob
  , hmmStatesForObservation :: obs -> (Int, Int -> state)
  , hmmMaxStatesForObservation :: Maybe Int
  }

funcArray :: (IArray a e, Ix i) => (i, i) -> (i -> e) -> a i e
funcArray rng func = listArray rng . map func $ range rng

-- | Create HMM model from dense input.
-- Uses O(N) memory where N is number of variables in model,
-- (for fast output generation).
makeDenseHmm
  :: forall s o p. (Num p, Ix s, IArray UArray p)
  => (s, s) -> [o]
  -> (s -> p) -> (s -> s -> p) -> (s -> o -> p)
  -> Hmm s o p
makeDenseHmm statesRange obs startProbs transProbs obsProbs =
  Hmm
  { hmmStartProbs = makeProbsFastRand states startProbs
  , hmmTransitionProbs = (transArr !)
  , hmmObservationProbs = (obsArr !)
  , hmmStatesForObservation = const (numStates, (statesArr !))
  , hmmMaxStatesForObservation = Just numStates
  }
  where
    numStates = rangeSize statesRange
    states = range statesRange
    statesArr :: Array Int s
    statesArr = listArray (0, numStates - 1) states
    transArr :: Array s (Probs s p)
    transArr = funcArray statesRange (makeProbsFastRand states . transProbs)
    obsArr :: Array s (Probs o p)
    obsArr = funcArray statesRange (makeProbsFastRand obs . obsProbs)

hmmGenerate
  :: (MonadRandom m, Fractional prob, Ord prob, Random prob)
  => Hmm state obs prob -> m [obs]
hmmGenerate hmm
  = probsRandom (hmmStartProbs hmm)
  >>= addObs
  >>= liftM (map snd)
    . (toList :: Monad m => ListT m a -> m [a])
    . iterateM step
  where
    addObs state
      = liftM ((,) state)
      . probsRandom $ hmmObservationProbs hmm state
    step (state, _)
      = probsRandom (hmmTransitionProbs hmm state)
      >>= addObs

data HmmCalc state prob =
  HmmCalc
  { hmmCForward :: Int -> state -> prob
  , hmmCBackward :: Int -> state -> prob
  , hmmCLayerMult :: Int -> prob
  , hmmCViterbi :: [state]
  }

hmmCLayerProbs :: Num prob => HmmCalc state prob -> Int -> state -> prob
hmmCLayerProbs
  = (liftA2 . liftA2) (fmap . (*)) hmmCLayerMult
  $ (liftA2 . liftA2 . liftA2) (*) hmmCForward hmmCBackward

hmmCalc :: Hmm state obs prob -> [obs] -> HmmCalc state prob
hmmCalc = undefined


{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Numeric.HMM
  ( Hmm(..), HmmLayerDesc(..), HmmFwdBwd(..)
  , makeDenseHmm
  , hmmLayerProbs
  ) where

import Data.IndexRange
import Numeric.Probability.Discrete

import Control.Applicative (liftA2)
import Data.Array.IArray (IArray)
import Data.Array.Unboxed (UArray)
import Data.Bijection

data HmmLayerDesc state =
  HmmLayerDesc
  { hmmLayerSize :: Int
  , hmmLayerStates :: Int -> state
  , hmmLayerTransitionsFromPrev :: state -> [Int]
  }

data Hmm state obs prob =
  Hmm
  { hmmStartProbs :: Probs state prob
  , hmmTransitionProbs :: state -> Probs state prob
  , hmmObservationProbs :: state -> Probs obs prob
  , hmmStatesForObservation :: obs -> HmmLayerDesc state
  }

-- | Create HMM model from dense input.
-- Uses O(N) memory where N is number of variables in model,
-- (for fast output generation).
makeDenseHmm
  :: forall s o p. (Floating p, IArray UArray p)
  => IndexRange s -> IndexRange o
  -> (s -> p) -> (s -> s -> p) -> (s -> o -> p)
  -> Hmm s o p
makeDenseHmm statesRange obsRange startProbs transProbs obsProbs =
  Hmm
  { hmmStartProbs = makeProbsFast statesRange startProbs
  , hmmTransitionProbs =
      irMemo statesRange (makeProbsFast statesRange . transProbs)
  , hmmObservationProbs =
      irMemo statesRange (makeProbsFast obsRange . obsProbs)
  , hmmStatesForObservation =
      const
      HmmLayerDesc
      { hmmLayerSize = irSize statesRange
      , hmmLayerStates = biTo (irIndex statesRange)
      , hmmLayerTransitionsFromPrev = const [0 .. irSize statesRange - 1]
      }
  }

data HmmFwdBwd state prob =
  HmmFwdBwd
  { -- | Forward algorithm. Including current layer's weight.
    -- Each layer is normalized.
    hmmForward :: Int -> state -> prob
  , -- | Backward algorithm. Not including current layer's weights
    -- Each layer is normalized.
    hmmBackward :: Int -> state -> prob
  , -- | Probabilty for observation according to model.
    hmmLogProb :: prob
  , hmmLayerMult :: Int -> prob
  }

hmmLayerProbs :: Num prob
  => HmmFwdBwd state prob -> Int -> state -> prob
hmmLayerProbs
  = (liftA2 . liftA2) (fmap . (*)) hmmLayerMult
  $ (liftA2 . liftA2 . liftA2) (*) hmmForward hmmBackward


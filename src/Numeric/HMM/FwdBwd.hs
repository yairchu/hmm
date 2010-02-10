{-# LANGUAGE FlexibleContexts #-}

module Numeric.HMM.FwdBwd
  ( HmmFwdBwd(..)
  , hmmFwdBwd
  , hmmLayerProbs
  ) where

import Data.Array.Unboxed.YC
import Numeric.HMM
import Numeric.HMM.Internal

import Control.Applicative (liftA2)

data HmmFwdBwd state prob =
  HmmFwdBwd
  { hmmLayers :: Int -> HmmLayerDesc state
  , -- | Forward algorithm. Including current layer's weight.
    -- Each layer is normalized.
    hmmForward :: Int -> Int -> prob
  , -- | Backward algorithm. Not including current layer's weights
    -- Each layer is normalized.
    hmmBackward :: Int -> Int -> prob
  , -- | Probabilty for observation according to model.
    hmmLogProb :: prob
  , hmmLayerMult :: Int -> prob
  }

hmmLayerProbs :: Num prob
  => HmmFwdBwd state prob -> Int -> Int -> prob
hmmLayerProbs
  = (liftA2 . liftA2) (fmap . (*)) hmmLayerMult
  $ (liftA2 . liftA2 . liftA2) (*) hmmForward hmmBackward

hmmFwdBwd
  :: (Unboxed prob, Num prob)
  => Hmm state obs prob -> Int -> (Int -> obs) -> HmmFwdBwd state prob
hmmFwdBwd model numObs getObs =
  HmmFwdBwd
  { hmmLayers = hmmStatesForObservation model . getObs
  , hmmBackward = backwardAlgorithmH l1Mode model $ map getObs [0 .. numObs - 1]
  }


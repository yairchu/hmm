{-# LANGUAGE FlexibleContexts #-}

module Numeric.HMM.FwdBwd
  ( HmmFwdBwd(..)
  , hmmLayerProbs
  ) where

import Numeric.HMM
import Numeric.HMM.Internal

import Control.Applicative (liftA2)

data HmmFwdBwd state prob =
  HmmFwdBwd
  { hmmFwdBwdNumLayers :: Int
  , -- | Forward algorithm. Including current layer's weight.
    -- Each layer is normalized.
    hmmForward :: Int -> state -> prob
  , -- | Backward algorithm. Not including current layer's weights
    -- Each layer is normalized.
    hmmBackward :: Int -> state -> prob
  , -- | Probabilty for observation according to model.
    hmmLogProb :: prob
  , hmmLayerMult :: Int -> prob
  , hmmLayers :: Int -> HmmLayerDesc state
  }

hmmLayerProbs :: Num prob
  => HmmFwdBwd state prob -> Int -> state -> prob
hmmLayerProbs
  = (liftA2 . liftA2) (fmap . (*)) hmmLayerMult
  $ (liftA2 . liftA2 . liftA2) (*) hmmForward hmmBackward


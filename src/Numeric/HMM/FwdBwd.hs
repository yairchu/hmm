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
  { -- | Forward algorithm. Including current layer's weight.
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
  :: (Unboxed prob, Floating prob)
  => Hmm state obs prob -> [obs] -> HmmFwdBwd state prob
hmmFwdBwd model observations =
  HmmFwdBwd
  { hmmLogProb = score
  , hmmBackward = backward
  }
  where
    (score, backward) = algoH Backwards l1Mode model observations


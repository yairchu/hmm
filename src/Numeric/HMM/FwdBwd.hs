{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Numeric.HMM.FwdBwd
  ( HmmFwdBwd(..)
  , hmmFwdBwd
  , hmmLayerProbs
  ) where

import Data.Array.Unboxed.YC
import Numeric.HMM
import Numeric.HMM.Internal

import Control.Applicative (liftA2)
import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)

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
  :: forall state obs prob. (Unboxed prob, Floating prob)
  => Hmm state obs prob -> [obs] -> HmmFwdBwd state prob
hmmFwdBwd model observations =
  HmmFwdBwd
  { hmmLogProb = score
  , hmmBackward = backward
  , hmmForward = forward
  , hmmLayerMult = (layerMult !)
  }
  where
    (score, backward) = algoH Backwards l1Mode model observations
    (_, forward) = algoH Forwards l1Mode model observations
    layerMult :: UArray Int prob
    layerMult
      = listArray (0, length observations - 1)
      $ zipWith calcLayerMult [0 ..] observations
    calcLayerMult i o
      = (1 /)
      . sum
      . map (liftA2 (*) (forward i) (backward i))
      $ [0 .. hmmLayerSize (hmmStatesForObservation model o) - 1]


module Numeric.HMM.Sparse
  ( Hmm(..), HmmCalc(..)
  , hmmCLayerProbs, hmmCalc
  ) where

import Numeric.Probability.Discrete

import Control.Applicative (liftA2)

data Hmm state obs prob =
  Hmm
  { hmmTransitionProbs :: state -> Probs state prob
  , hmmObserveProbs :: state -> obs -> prob
  , hmmStartProbs :: [(state, prob)]
  , hmmStatesForObservation :: obs -> (Int, Int -> state)
  , hmmMaxStatesForObservation :: Maybe Int
  }

{-
hmmGenerate
  :: (Fractional prob, Random random)
  => Hmm state obs prob -> random -> [obs]
hmmGenerate hmm rand =
  where
    startState = 
-}

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


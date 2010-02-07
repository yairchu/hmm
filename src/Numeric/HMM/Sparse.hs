module Numeric.HMM.Sparse
  ( Hmm(..), HmmCalc(..)
  , hmmCLayerProbs, hmmCalc, hmmGenerate
  ) where

import Numeric.Probability.Discrete

import Control.Applicative (liftA2)
import Control.Monad (liftM)
import Control.Monad.ListT (ListT)
import Control.Monad.Random.Class (MonadRandom)
import Data.List.Class (iterateM, toList)
import System.Random (Random)

data Hmm state obs prob =
  Hmm
  { hmmTransitionProbs :: state -> Probs state prob
  , hmmObserveProbs :: state -> Probs obs prob
  , hmmStartProbs :: Probs state prob
  , hmmStatesForObservation :: obs -> (Int, Int -> state)
  , hmmMaxStatesForObservation :: Maybe Int
  }

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
      . probsRandom $ hmmObserveProbs hmm state
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


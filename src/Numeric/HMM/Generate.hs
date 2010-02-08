module Numeric.HMM.Generate
  ( hmmGenerate
  ) where

import Numeric.HMM (Hmm(..))
import Numeric.Probability.Discrete (probsRandom)

import Control.Monad (liftM)
import Control.Monad.ListT (ListT)
import Control.Monad.Random.Class (MonadRandom)
import Data.List.Class (iterateM, toList)
import System.Random (Random)

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


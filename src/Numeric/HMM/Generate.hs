module Numeric.HMM.Generate
  ( hmmGenerate
  , hmmGenerateObservations
  , hmmGenerateHiddenStates
  ) where

import Numeric.HMM (Hmm(..))
import Numeric.Probability.Discrete (probsRandom)

import Control.Applicative ((<*>))
import Control.Monad.ListT (ListT)
import Control.Monad.Random.Class (MonadRandom)
import Data.List.Class (iterateM, joinM)
import System.Random (Random)

class (Fractional a, Ord a, Random a) => SuitableRandom a

hmmGenerateHiddenStates
  :: (MonadRandom m, SuitableRandom prob)
  => Hmm state obs prob -> ListT m state
hmmGenerateHiddenStates hmm =
  iterateM
  (probsRandom . hmmTransitionProbs hmm)
  ((probsRandom . hmmStartProbs) hmm)

hmmGenerateObservations
  :: (MonadRandom m, SuitableRandom prob)
  => Hmm state obs prob -> ListT m state -> ListT m obs
hmmGenerateObservations hmm =
  joinM . fmap (probsRandom . hmmObservationProbs hmm)

hmmGenerate
  :: (MonadRandom m, SuitableRandom prob)
  => Hmm state obs prob -> ListT m obs
hmmGenerate
  = hmmGenerateObservations
  <*> hmmGenerateHiddenStates


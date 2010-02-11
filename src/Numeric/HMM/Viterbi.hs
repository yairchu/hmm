module Numeric.HMM.Viterbi
  ( ViterbiResult(..)
  , viterbi, viterbiPathCertainty
  ) where

import Data.Array.Unboxed.YC (Unboxed(..))
import Numeric.HMM (Hmm(..), HmmLayerDesc(..))
import Numeric.HMM.FwdBwd (HmmFwdBwd(..))
import Numeric.HMM.Internal (Direction(Backwards), algoH, logLInfMode)
import Numeric.Probability.Discrete (probsLog)

import Data.List (maximumBy)
import Data.Ord (comparing)

data ViterbiResult state prob =
  ViterbiResult
  { viterbiPath :: [state]
  , viterbiLogProb :: prob
  }

viterbiPathCertainty :: Num prob
  => ViterbiResult state prob -> HmmFwdBwd state prob -> prob
viterbiPathCertainty v fb = viterbiLogProb v - hmmLogProb fb

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn = maximumBy . comparing

viterbi
  :: (Unboxed prob, Floating prob, Ord prob)
  => Hmm state obs prob -> [obs] -> ViterbiResult state prob
viterbi model observations =
  last res `seq`
  ViterbiResult
  { viterbiPath = res
  , viterbiLogProb = score
  }
  where
    res = scanl step start . tail $ zip [0..] layers
    layers = map (hmmStatesForObservation model) observations
    (score, bwd) = algoH Backwards logLInfMode model observations
    firstLayer = head layers
    startIdx = maximumOn (bwd 0) [0 .. hmmLayerSize firstLayer]
    start = hmmLayerStates firstLayer startIdx
    step prevState (layerIdx, layer) =
      hmmLayerStates layer
      . maximumOn f $ hmmLayerTransitionsFromPrev layer prevState
      where
        transition
          = probsLog (hmmTransitionProbs model prevState)
          . hmmLayerStates layer
        f i = bwd layerIdx i + transition i


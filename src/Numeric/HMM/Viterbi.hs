module Numeric.HMM.Viterbi
  ( ViterbiResult(..)
  , viterbi, viterbiPathCertainty
  ) where

import Data.Array.Unboxed.YC
import Numeric.HMM
import Numeric.Probability.Discrete

import Control.Applicative ((<$>), (<*>), ZipList(..))
import Control.Monad (forM_)
import Control.Monad.ST
import Data.STRef

data ViterbiResult state prob =
  ViterbiResult
  { viterbiPath :: [state]
  , viterbiLogProb :: prob
  }

viterbiPathCertainty :: Num prob
  => ViterbiResult state prob -> HmmFwdBwd state prob -> prob
viterbiPathCertainty v fb = viterbiLogProb v - hmmLogProb fb

viterbi
  :: (Unboxed prob, Floating prob, Ord prob)
  => Hmm state obs prob -> [obs] -> ViterbiResult state prob
viterbi model observations = runST $ do
  arr <- newSTUArray (0, arrSize - 1) 0
  let
    addLayerWeights arrIdx layer probFunc =
      forM_ [0 .. hmmLayerSize layer - 1] $ \i ->
        modifySTUArray arr (arrIdx + i) (+ probFunc (hmmLayerStates layer i))
    addLayerObsWeights arrIdx layer obser =
      addLayerWeights arrIdx layer
        ((`probsLog` obser) . hmmObservationProbs model)
  arrIdxRef <- newSTRef 0
  forM_
    (getZipList ((,,)
    <$> ZipList revLayers
    <*> ZipList (tail revLayers)
    <*> ZipList (reverse observations)
    )) $ \(layerN, layerP, obsN) -> do
      arrIdxN <- readSTRef arrIdxRef
      let arrIdxP = arrIdxN + hmmLayerSize layerN
      writeSTRef arrIdxRef arrIdxP
      addLayerObsWeights arrIdxN layerN obsN
      forM_ [0 .. hmmLayerSize layerP] $ \iP -> do
        let
          stateP = hmmLayerStates layerP iP
          (iN0 : iNs) = hmmLayerTransitionsFromPrev layerN stateP
        readSTUArray arr (arrIdxN + iN0)
          >>= writeSTUArray arr (arrIdxP + iP)
        forM_ iNs $ \iN ->
          readSTUArray arr (arrIdxN + iN)
            >>= modifySTUArray arr (arrIdxP + iP) . max
  layer0Idx <- readSTRef arrIdxRef
  addLayerObsWeights layer0Idx (head layers) (head observations)
  addLayerWeights layer0Idx (head layers) . probsLog $ hmmStartProbs model
  undefined
  where
    arrSize = sum $ map hmmLayerSize layers
    layers = map (hmmStatesForObservation model) observations
    revLayers = reverse layers
    -- nodeWeights = probsLogs $ 


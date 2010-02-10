{-# LANGUAGE FlexibleContexts #-}

module Numeric.HMM.Viterbi
  ( ViterbiResult(..)
  , viterbi, viterbiPathCertainty
  ) where

import Data.Array.Unboxed.YC (Unboxed(..), modifySTUArray)
import Numeric.HMM (Hmm(..), HmmLayerDesc(..), HmmFwdBwd(..))
import Numeric.Probability.Discrete (probsLog)

import Control.Applicative ((<$>), (<*>), ZipList(..))
import Control.Monad (forM_)
import Data.Array.ST (runSTUArray)
import Data.Array.IArray (IArray, (!))
import Data.Array.Unboxed (UArray)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

data ViterbiResult state prob =
  ViterbiResult
  { viterbiPath :: [state]
  , viterbiLogProb :: prob
  }

viterbiPathCertainty :: Num prob
  => ViterbiResult state prob -> HmmFwdBwd state prob -> prob
viterbiPathCertainty v fb = viterbiLogProb v - hmmLogProb fb

viterbiArr
  :: (Unboxed prob, Floating prob, Ord prob)
  => Hmm state obs prob -> [obs]
  -> [HmmLayerDesc state] -> UArray Int prob
viterbiArr model observations layers = runSTUArray $ do
  let arrSize = sum $ map hmmLayerSize layers
  arr <- newSTUArray (0, arrSize - 1) 0
  let
    addLayerWeights arrIdx layer probFunc =
      forM_ [0 .. hmmLayerSize layer - 1] $ \i ->
        modifySTUArray arr (arrIdx + i) (+ probFunc (hmmLayerStates layer i))
    addLayerObsWeights arrIdx layer obser =
      addLayerWeights arrIdx layer
        ((`probsLog` obser) . hmmObservationProbs model)
  arrIdxRef <- newSTRef 0
  let revLayers = reverse layers
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
          transition
            = probsLog (hmmTransitionProbs model stateP)
            . hmmLayerStates layerN
          (iN0 : iNs) = hmmLayerTransitionsFromPrev layerN stateP
        readSTUArray arr (arrIdxN + iN0)
          >>= writeSTUArray arr (arrIdxP + iP)
        forM_ iNs $ \iN ->
          readSTUArray arr (arrIdxN + iN)
            >>= modifySTUArray arr (arrIdxP + iP)
              . max . (+ transition iN)
  layer0Idx <- readSTRef arrIdxRef
  addLayerObsWeights layer0Idx (head layers) (head observations)
  addLayerWeights layer0Idx (head layers) . probsLog $ hmmStartProbs model
  return arr

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn = maximumBy . comparing

viterbi
  :: (IArray UArray prob, Unboxed prob, Floating prob, Ord prob)
  => Hmm state obs prob -> [obs] -> ViterbiResult state prob
viterbi model observations =
  last res `seq`
  logProb `seq`
  ViterbiResult
  { viterbiPath = res
  , viterbiLogProb = logProb
  }
  where
    logProb = arr ! startIdx
    res = scanl step start $ tail layersInfo
    layers = map (hmmStatesForObservation model) observations
    arr = viterbiArr model observations layers
    layersInfo = zip layers . scanl (+) 0 $ map hmmLayerSize layers
    firstLayer = head layers
    startIdx = maximumOn (arr !) $ [0 .. hmmLayerSize firstLayer]
    start = hmmLayerStates firstLayer startIdx
    step prevState (layer, layerArrIdx) =
      hmmLayerStates layer
      . maximumOn f $ [0 .. hmmLayerSize layer]
      where
        transition
          = probsLog (hmmTransitionProbs model prevState)
          . hmmLayerStates layer
        f i = (arr ! (layerArrIdx + i)) + transition i


{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Numeric.HMM.Internal
  ( backwardAlgorithmH, l1Mode, logLInfMode
  ) where

import Data.Array.Unboxed.YC
import Numeric.HMM
import Numeric.Probability.Discrete (Probs(probsFunc), probsLog)

import Control.Applicative
import Control.Monad (forM_)
import Control.Monad.ListT (ListT)
import Control.Monad.ST (ST)
import Data.Array.IArray ((!), listArray)
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.List.Class hiding (scanl)

data AlgoMode p =
  AlgoMode
  { algoAdd :: p -> p -> p
  , algoMult :: p -> p -> p
  , algoInverse :: p -> p
  , algoVal :: forall s. Probs s p -> s -> p
  }

-- for normal forward/backward algorithms
l1Mode :: Fractional a => AlgoMode a
l1Mode = AlgoMode (+) (*) (1 /) probsFunc

-- for viterbi
logLInfMode :: (Ord a, Floating a) => AlgoMode a
logLInfMode = AlgoMode max (+) negate probsLog

backwardAlgorithmH
  :: forall state obs prob. Unboxed prob
  => AlgoMode prob -> Hmm state obs prob -> [obs]
  -> Int -> Int -> prob
backwardAlgorithmH mode hmm observations =
  getVal
  where
    getVal layer node = resultArr ! ((layerArrIdxs ! layer) + node)
    layers = map (hmmStatesForObservation hmm) observations
    numLayers = length layers
    layerArrIdxs :: UArray Int Int
    layerArrIdxs = listArray (0, numLayers - 1) . scanl (+) 0 . map hmmLayerSize $ init layers
    arrSize = sum $ map hmmLayerSize layers
    nodeWeight st obser True
      = algoAdd mode
        (nodeWeight st obser False)
        (algoVal mode (hmmStartProbs hmm) st)
    nodeWeight st obser False
      = algoVal mode (hmmObservationProbs hmm st) obser
    revLayers = reverse layers
    revObs = reverse observations
    lastLayer = head revLayers
    frmList :: forall a s. [a] -> ListT (ST s) a
    frmList = fromList
    normalizeLayer arr layer layerIdx = do
      let
        idxs =
          map (+ (layerArrIdxs ! layerIdx))
          [0 .. hmmLayerSize layer - 1]
      tot
        <- foldl1L (algoAdd mode)
        . joinM . fmap (readSTUArray arr)
        . frmList $ idxs
      let
        mult = algoInverse mode tot
        normalize i = modifySTUArray arr i $ algoMult mode mult
      forM_ idxs normalize
      return tot
    resultArr :: UArray Int prob
    resultArr = runSTUArray $ do
      arr <- newSTUArrayDef (0, arrSize - 1)
      forM_ [0 .. hmmLayerSize lastLayer - 1] $ \i ->
        writeSTUArray arr ((layerArrIdxs ! (numLayers - 1)) + i)
          $ nodeWeight (hmmLayerStates lastLayer i) (head revObs) False
      normalizeLayer arr lastLayer (numLayers - 1)
      forM_
        (getZipList $ (,,,)
        <$> ZipList [numLayers - 2, numLayers - 3 .. 0]
        <*> ZipList (tail revObs)
        <*> ZipList (tail revLayers)
        <*> ZipList revLayers
        ) $ \(layerIdxP, obsP, layerP, layerN) -> do
        let
          arrIdxP = layerArrIdxs ! layerIdxP
          arrIdxN = layerArrIdxs ! (layerIdxP+1)
        forM_ [0 .. hmmLayerSize layerP] $ \iP ->
          let
            stateP = hmmLayerStates layerP iP
            incoming
              = joinM . fmap proc . frmList
              . hmmLayerTransitionsFromPrev layerN
              $ stateP
            proc iN
              = fmap
                ( algoMult mode
                . algoVal mode
                  (hmmTransitionProbs hmm stateP)
                $ hmmLayerStates layerN iN
                )
              $ readSTUArray arr (arrIdxN + iN)
          in
            writeSTUArray arr (arrIdxP + iP)
            . (algoMult mode (nodeWeight stateP obsP (iP == 0)))
            =<< foldl1L (algoAdd mode) incoming
        normalizeLayer arr layerP layerIdxP
      return arr


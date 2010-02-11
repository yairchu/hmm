{-# LANGUAGE RankNTypes #-}

module Numeric.HMM.Internal
  ( Direction(..), algoH, l1Mode, logLInfMode
  ) where

import Data.Array.Unboxed.YC
import Numeric.HMM
import Numeric.Probability.Discrete (Probs(probsFunc), probsLog)

import Control.Applicative
import Control.Monad (forM_)
import Control.Monad.ListT (ListT)
import Control.Monad.ST (ST, runST)
import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)
import Data.List.Class hiding (scanl)
import Data.STRef

data AlgoMode p =
  AlgoMode
  { algoAdd :: p -> p -> p
  , algoMult :: p -> p -> p
  , algoInverse :: p -> p
  , algoZero :: p
  , algoToLog :: p -> p
  , algoVal :: forall s. Probs s p -> s -> p
  }

-- for normal forward/backward algorithms
l1Mode :: Floating a => AlgoMode a
l1Mode = AlgoMode (+) (*) (1 /) 0 log probsFunc

-- for viterbi
logLInfMode :: (Ord a, Floating a) => AlgoMode a
logLInfMode = AlgoMode max (+) negate undefined id probsLog

data Direction = Forwards | Backwards

listTfromList :: [a] -> ListT (ST s) a
listTfromList = fromList

algoH
  :: (Unboxed prob, Num prob)
  => Direction -> AlgoMode prob -> Hmm state obs prob -> [obs]
  -> (prob, Int -> Int -> prob)
algoH dir mode hmm observations =
  (score, getVal)
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
    algoOrder =
      case dir of
        Forwards -> id
        Backwards -> reverse
    ordLayers = algoOrder layers
    ordObs = algoOrder observations
    ordLayerIdxs = algoOrder [0 .. numLayers - 1]
    startLayer = head ordLayers
    startLayerIdx = head ordLayerIdxs
    normalizeLayer arr layer layerIdx = do
      let
        idxs =
          map (+ (layerArrIdxs ! layerIdx))
          [0 .. hmmLayerSize layer - 1]
      tot
        <- foldl1L (algoAdd mode)
        . joinM . fmap (readSTUArray arr)
        . listTfromList $ idxs
      let
        mult = algoInverse mode tot
        normalize i = modifySTUArray arr i $ algoMult mode mult
      forM_ idxs normalize
      let r = algoToLog mode tot
      r `seq` return r
    startLayerVal i =
      case dir of
        Forwards -> 1
        Backwards ->
          nodeWeight (hmmLayerStates startLayer i) (head ordObs) False
    backwardsLayer arr (layerIdxP, layerIdxN, obsP, _, layerP, layerN) =
      forM_ [0 .. hmmLayerSize layerP] $ \iP ->
        let
          stateP = hmmLayerStates layerP iP
          incoming
            = joinM . fmap proc . listTfromList
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
          . algoMult mode (nodeWeight stateP obsP (layerIdxP == 0))
          =<< foldl1L (algoAdd mode) incoming
      where
        arrIdxP = layerArrIdxs ! layerIdxP
        arrIdxN = layerArrIdxs ! layerIdxN
    forwardsLayer arr (layerIdxN, layerIdxP, _, obsP, layerN, layerP) = do
      let
        arrIdxN = layerArrIdxs ! layerIdxN
        arrIdxP = layerArrIdxs ! layerIdxP
      forM_ [0 .. hmmLayerSize layerN - 1] $ \iN ->
        writeSTUArray arr (arrIdxN + iN) (algoZero mode)
      forM_ [0 .. hmmLayerSize layerP - 1] $ \iP -> do
        valP <- readSTUArray arr (arrIdxP + iP)
        let
          stateP = hmmLayerStates layerP iP
          valPInc
            = algoMult mode valP
            $ nodeWeight stateP obsP (layerIdxP == 0)
        forM_ (hmmLayerTransitionsFromPrev layerN stateP) $ \iN ->
          let
            valToAdd
              = algoMult mode valPInc
              . algoVal mode
                (hmmTransitionProbs hmm stateP)
              $ hmmLayerStates layerN iN
          in
            modifySTUArray arr (arrIdxN + iN) $ algoAdd mode valToAdd
    (score, resultArr) = runST $ do
      arr <- newSTUArrayDef (0, arrSize - 1)
      forM_ [0 .. hmmLayerSize startLayer - 1] $ \i ->
        writeSTUArray arr ((layerArrIdxs ! startLayerIdx) + i)
          $ startLayerVal i
      scoreRef <- newSTRef
        =<< normalizeLayer arr startLayer startLayerIdx
      forM_
        (getZipList $ (,,,,,)
        <$> ZipList (tail ordLayerIdxs)
        <*> ZipList ordLayerIdxs
        <*> ZipList (tail ordObs)
        <*> ZipList ordObs
        <*> ZipList (tail ordLayers)
        <*> ZipList ordLayers
        ) $ \args@(layerIdx, _, _, _, layer, _) -> do
        case dir of
          Forwards -> forwardsLayer arr args
          Backwards -> backwardsLayer arr args
        layerNorm <- normalizeLayer arr layer layerIdx
        prevScore <- readSTRef scoreRef
        let newScore = prevScore + layerNorm
        newScore `seq` writeSTRef scoreRef newScore
      rScore <- readSTRef scoreRef
      rArr <- unsafeFreezeSTU arr
      return (rScore, rArr)


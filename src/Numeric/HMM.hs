{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Numeric.HMM
  ( Hmm(..), HmmFwdBwd(..), HmmViterbi(..)
  , makeDenseHmm
  , hmmGenerate
  , hmmLayerProbs
  , hmmViterbi, hmmViterbiCertainty
  ) where

import Data.Array.Unboxed.YC
import Data.IndexRange
import Numeric.Probability.Discrete

import Control.Applicative ((<$>), (<*>), ZipList(..), liftA2)
import Control.Monad (forM_, liftM)
import Control.Monad.ListT (ListT)
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.ST
import Data.Array.IArray (IArray)
import Data.Array.Unboxed (UArray)
import Data.Bijection
import Data.List.Class (iterateM, toList)
import Data.STRef
import System.Random (Random)

data HmmLayerDesc state =
  HmmLayerDesc
  { hmmLayerSize :: Int
  , hmmLayerStates :: Int -> state
  , hmmLayerTransitionsFromPrev :: state -> [Int]
  }

data Hmm state obs prob =
  Hmm
  { hmmStartProbs :: Probs state prob
  , hmmTransitionProbs :: state -> Probs state prob
  , hmmObservationProbs :: state -> Probs obs prob
  , hmmStatesForObservation :: obs -> HmmLayerDesc state
  }

-- | Create HMM model from dense input.
-- Uses O(N) memory where N is number of variables in model,
-- (for fast output generation).
makeDenseHmm
  :: forall s o p. (Floating p, IArray UArray p)
  => IndexRange s -> IndexRange o
  -> (s -> p) -> (s -> s -> p) -> (s -> o -> p)
  -> Hmm s o p
makeDenseHmm statesRange obsRange startProbs transProbs obsProbs =
  Hmm
  { hmmStartProbs = makeProbsFast statesRange startProbs
  , hmmTransitionProbs =
      irMemo statesRange (makeProbsFast statesRange . transProbs)
  , hmmObservationProbs =
      irMemo statesRange (makeProbsFast obsRange . obsProbs)
  , hmmStatesForObservation =
      const
      HmmLayerDesc
      { hmmLayerSize = irSize statesRange
      , hmmLayerStates = biTo (irIndex statesRange)
      , hmmLayerTransitionsFromPrev = const [0 .. irSize statesRange - 1]
      }
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
      . probsRandom $ hmmObservationProbs hmm state
    step (state, _)
      = probsRandom (hmmTransitionProbs hmm state)
      >>= addObs

data HmmFwdBwd state prob =
  HmmFwdBwd
  { -- | Forward algorithm. Including current layer's weight.
    -- Each layer is normalized.
    hmmForward :: Int -> state -> prob
  , -- | Backward algorithm. Not including current layer's weights
    -- Each layer is normalized.
    hmmBackward :: Int -> state -> prob
  , -- | Probabilty for observation according to model.
    hmmLogProb :: prob
  , hmmLayerMult :: Int -> prob
  }

data HmmViterbi state prob =
  HmmViterbi
  { hmmViterbiPath :: [state]
  , hmmViterbiLogProb :: prob
  }

hmmLayerProbs :: Num prob
  => HmmFwdBwd state prob -> Int -> state -> prob
hmmLayerProbs
  = (liftA2 . liftA2) (fmap . (*)) hmmLayerMult
  $ (liftA2 . liftA2 . liftA2) (*) hmmForward hmmBackward

hmmViterbiCertainty :: Num prob
  => HmmViterbi state prob -> HmmFwdBwd state prob -> prob
hmmViterbiCertainty v fb = hmmViterbiLogProb v - hmmLogProb fb

hmmViterbi
  :: (Unboxed prob, Floating prob, Ord prob)
  => Hmm state obs prob -> [obs] -> HmmViterbi state prob
hmmViterbi model observations = runST $ do
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


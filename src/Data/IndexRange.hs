{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

-- | IndexRange extends the functionality of Ix.

module Data.IndexRange
  ( IxR(..), IndexRange(..)
  , irRange, irMemo, irUMemo
  ) where

import Data.Bijection (Bijection(..))
import Data.Array.IArray (IArray, Array, (!), listArray)
import Data.Array.Unboxed (UArray)

data IndexRange i =
  IndexRange
  { irSize :: Int
  , irIndex :: Bijection (->) Int i
  }

irRange :: IndexRange i -> [i]
irRange ir = map ((biTo . irIndex) ir) [0 .. irSize ir - 1]

irMemoDummy
  :: forall a i e. IArray a e
  => a Int e -> IndexRange i -> (i -> e) -> i -> e
irMemoDummy _ ir func =
  (arr !) . biFrom (irIndex ir)
  where
    arr :: a Int e
    arr
      = listArray (0, irSize ir - 1)
      . map func $ irRange ir

irMemo :: forall i a. IndexRange i -> (i -> a) -> i -> a
irMemo = irMemoDummy (undefined :: Array Int a)

irUMemo
  :: forall i a. IArray UArray a
  => IndexRange i -> (i -> a) -> i -> a
irUMemo = irMemoDummy (undefined :: UArray Int a)

class IxR i where
  indexRangeFromBounds :: (i, i) -> IndexRange i

instance Integral a => IxR a where
  indexRangeFromBounds (start, end) =
    IndexRange
    { irSize = fromIntegral $ end + 1 - start
    , irIndex =
        Bi
        ((+ start) . fromIntegral)
        (fromIntegral . (+ negate start))
    }


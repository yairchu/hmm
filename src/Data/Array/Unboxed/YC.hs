{-# LANGUAGE FlexibleContexts #-}

module Data.Array.Unboxed.YC
  ( Unboxed(..)
  , modifySTUArray
  ) where

import Control.Monad.ST (ST)
import Data.Array.IArray (IArray)
import Data.Array.MArray (newArray, readArray, writeArray, unsafeFreeze)
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Ix (Ix)

class IArray UArray a => Unboxed a where
  newSTUArray :: Ix i => (i, i) -> a -> ST s (STUArray s i a)
  newSTUArrayDef :: Ix i => (i, i) -> ST s (STUArray s i a)
  readSTUArray :: Ix i => STUArray s i a -> i -> ST s a
  writeSTUArray :: Ix i => STUArray s i a -> i -> a -> ST s ()
  unsafeFreezeSTU :: Ix i => STUArray s i a -> ST s (UArray i a)

instance Unboxed Float where
  newSTUArray = newArray
  newSTUArrayDef = (`newArray` 0)
  readSTUArray = readArray
  writeSTUArray = writeArray
  unsafeFreezeSTU = unsafeFreeze

instance Unboxed Double where
  newSTUArray = newArray
  newSTUArrayDef = (`newArray` 0)
  readSTUArray = readArray
  writeSTUArray = writeArray
  unsafeFreezeSTU = unsafeFreeze

modifySTUArray :: (Unboxed a, Ix i) => STUArray s i a -> i -> (a -> a) -> ST s ()
modifySTUArray arr idx func =
  readSTUArray arr idx >>= writeSTUArray arr idx . func


-- | This module implements a growing buffer where
-- each element can be appended relatively efficiently.
{-# LANGUAGE BangPatterns #-}
module Codec.Volume.VectorOutput
    ( VectorOut
    , newVectorOut
    , prevValueAt
    , unsafePrevValueAt
    , finalizeOutput
    {-, written-}
    , push
    ) where

import Data.STRef
import Control.Monad.Primitive
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector.Storable.Mutable( Storable )

-- | Gathering buffer, implemented using a mutable vector.
data VectorOut s a = VectorOut
  { _outIndex :: !(STRef s Int)
  , _outVector :: !(STRef s (VSM.MVector s a))
  }

-- | Create a new vector output, in IO or ST monad.
newVectorOut :: (Storable a, PrimMonad m) => m (VectorOut (PrimState m) a)
newVectorOut =
  primToPrim $ VectorOut <$> newSTRef 0 <*> (VSM.new (64 * 64 * 64) >>= newSTRef)

-- | When the vertice gathering is finished, get back an imutable
-- vector of the value.
finalizeOutput :: (Storable a, PrimMonad m)
               => VectorOut (PrimState m) a -> m (VS.Vector a)
finalizeOutput o = primToPrim $ do
  ix <- readSTRef $ _outIndex o
  vec <- readSTRef $ _outVector o
  VS.unsafeFreeze $ VSM.take ix vec

{-written :: PrimMonad m => m Int-}
{-written = primToPrim $ readSTRef $ _outIndex o-}

prevValueAt :: (Storable a, PrimMonad m, Applicative m)
            => VectorOut (PrimState m) a -> Int -> a -> m a
prevValueAt o delta def = primToPrim $ do
  ix <- readSTRef $ _outIndex o
  if delta > ix then
    pure def
  else do
    vec <- readSTRef $ _outVector o
    vec `VSM.read` (ix - delta)

unsafePrevValueAt :: (Storable a, PrimMonad m, Applicative m)
                  => VectorOut (PrimState m) a -> Int -> m a
unsafePrevValueAt o delta = primToPrim $ do
  ix <- readSTRef $ _outIndex o
  vec <- readSTRef $ _outVector o
  vec `VSM.unsafeRead` (ix - delta)

-- | Add an element at the end of the vector, return the writing index.
push :: (Storable a, PrimMonad m) => VectorOut (PrimState m) a -> a -> m Int
{-# INLINE push #-}
push o e = primToPrim $ do
    !ix <- readSTRef $ _outIndex o
    !vec <- readSTRef $ _outVector o
    if ix >= VSM.length vec then do
      let !vSize = VSM.length vec * 2
      !newVec <- VSM.grow vec vSize 
      writeSTRef (_outVector o) newVec
      go ix newVec
    else
      go ix vec
  where
    go !ix !vec = do
      writeSTRef (_outIndex o) $! ix + 1
      VSM.unsafeWrite vec ix e
      return ix


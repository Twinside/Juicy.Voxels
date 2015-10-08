{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Codec.Volume.VectorByteConversion( blitVector, toByteString ) where

import Data.Word( Word8 )
import Data.Vector.Storable( Vector, unsafeToForeignPtr, unsafeFromForeignPtr0 )
import Foreign.Storable( Storable, sizeOf )

#if !MIN_VERSION_base(4,8,0)
import Foreign.ForeignPtr.Safe( ForeignPtr, castForeignPtr )
#else
import Foreign.ForeignPtr( ForeignPtr, castForeignPtr )
#endif


import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as S

blitVector :: Vector Word8 -> Int -> Int -> B.ByteString
blitVector vec atIndex = S.PS ptr (offset + atIndex)
  where (ptr, offset, _length) = unsafeToForeignPtr vec

toByteString :: forall a. (Storable a) => Vector a -> B.ByteString
toByteString vec = S.PS (castForeignPtr ptr) offset (len * size)
  where (ptr, offset, len) = unsafeToForeignPtr vec
        size = sizeOf (undefined :: a)


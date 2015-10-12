{-# LANGUAGE TypeFamilies #-}
module Codec.Volume.Types
    ( VoxelSize( .. )
    , VolumeSize( .. )
    , Volume( .. )
    , DynamicVolume( .. )
    , samplesInVolume
    , slice
    , slices
    , volumeMap
    , endianSwap
    ) where

import Data.Bits( (.|.), (.&.), unsafeShiftL, unsafeShiftR )
import Codec.Picture( Image( .. ), PixelBaseComponent )
import Data.Word( Word8, Word16 )
import qualified Data.Vector.Storable as VS

-- | Voxel size for non-square ones.
data VoxelSize a = VoxelSize
  { _voxelWidth  :: !a
  , _voxelHeight :: !a
  , _voxelDepth  :: !a
  }
  deriving (Eq, Show, Ord)

data VolumeSize = VolumeSize
  { _volumeWidth  :: !Int
  , _volumeHeight :: !Int
  , _volumeDepth  :: !Int
  }
  deriving (Eq, Show, Ord)

samplesInVolume :: VolumeSize -> Int
samplesInVolume vs =
  _volumeWidth vs * _volumeHeight vs * _volumeDepth vs

data Volume a = Volume
  { _volumeSize  :: !VolumeSize
  , _volumeVoxel :: !(VoxelSize Float)
  , _volumeData  :: !(VS.Vector a)
  }
  deriving (Eq, Ord)

volumeMap :: (VS.Storable a, VS.Storable b)
          => (a -> b) -> Volume a -> Volume b
volumeMap f vol = vol { _volumeData = VS.map f $ _volumeData vol }

-- | Extract a 2D image which is a slice of the volume.
-- expect `O(1)`
slice :: (VS.Storable a, a ~ PixelBaseComponent a)
      => Volume a -> Int -> Image a
slice vol ix = Image w h imgData where
  VolumeSize w h _d = _volumeSize vol
  imgData = VS.take (w * h) . VS.drop (w * h * ix) $ _volumeData vol

-- | Extract all the 2D image from of volume.
-- `O(depth)`
slices :: (VS.Storable a, a ~ PixelBaseComponent a)
       => Volume a -> [Image a]
slices vol = slice vol <$> [0 .. _volumeDepth (_volumeSize vol) - 1]

data DynamicVolume
  = Volume8  (Volume Word8)
  | Volume16 (Volume Word16)
  deriving (Eq, Ord)

endianSwap :: Word16 -> Word16
{-# INLINE endianSwap #-}
endianSwap a = (a `unsafeShiftR` 8) .|. ((a .&. 0xFF) `unsafeShiftL` 8)


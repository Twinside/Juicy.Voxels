module Codec.Volume.Types where

import qualified Data.Vector.Storable as VS

data Volume a = Volume
  { _volumeWidth  :: !Int
  , _volumeHeight :: !Int
  , _volumeDepth  :: !Int
  , _volumeData   :: !(VS.Vector a)
  }


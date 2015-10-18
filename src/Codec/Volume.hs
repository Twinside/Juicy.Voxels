module Codec.Volume
  ( -- * Types
    Volume( .. )
  , DynamicVolume( .. )
    -- * Input/Output functions
    -- ** PVM
  , readPVM
  , decodePVM
    -- ** GIF
  , encodeGifVolume
  , writeGifVolume

    -- * Helper functions
  , slice
  , slices
  , volumeMap
  ) where

import Codec.Volume.Types
import Codec.Volume.Pvm
import Codec.Volume.Gif


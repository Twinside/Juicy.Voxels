-- | Loader for PVM volume files or volume stored in GIFs or image
-- list.
--
-- Most important functions are `readPVM` and `volumeFromImageFiles`.
module Codec.Volume
  ( -- * Types
    Volume( .. )
  , DynamicVolume( .. )
    -- * Input/Output functions
    -- ** PVM
  , readPVM
  , decodePVM

    -- ** GIF
  , decodeGifVolume
  , encodeGifVolume
  , writeGifVolume

    -- ** Images
  , volumeOfImages
  , volumeFromDynamicImages
  , volumeFromImageFiles 

    -- * Helper functions
  , slice
  , slices
  , volumeMap
  ) where

import Codec.Volume.Types
import Codec.Volume.Pvm
import Codec.Volume.Pictures


-- | Store a volume into a greyscale Gif animation.
-- Due to the gif format specification, the resolution is limited
-- to 8 bits.
{-# LANGUAGE TupleSections #-}
module Codec.Volume.Gif
    ( encodeGifVolume
    , writeGifVolume
    ) where

import Data.Word( Word8 )
import qualified Data.ByteString.Lazy as LB
import Codec.Picture.Gif( encodeGifImages
                        , GifLooping( LoopingForever )
                        , greyPalette )
import Codec.Volume.Types

-- | Encode a volume as a GIF image
encodeGifVolume :: Volume Word8 -> Either String LB.ByteString
encodeGifVolume vol = 
  encodeGifImages LoopingForever $ (greyPalette, 7,) <$> slices vol


-- | Write a volume as a GIF file.
writeGifVolume :: FilePath -> Volume Word8 -> Either String (IO ())
writeGifVolume fname volume = LB.writeFile fname <$> encodeGifVolume volume


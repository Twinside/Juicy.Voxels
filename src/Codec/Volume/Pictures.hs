{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Codec.Volume.Pictures
    ( writeGifVolume 
    , decodeGifVolume
    , encodeGifVolume
    , volumeOfImages
    , volumeFromDynamicImages
    , volumeFromImageFiles 
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.List( partition )
import qualified Data.Vector.Storable as VSM
import Data.Word( Word8, Word16 )
import Codec.Picture
    ( Pixel8
    , Pixel16
    , PixelYA16( .. )
    , PixelRGBA16( .. )
    , Image( .. )
    , DynamicImage( .. )
    , PixelBaseComponent( .. )
    , pixelMap
    , readImage
    )
import Codec.Picture.Types( extractLumaPlane )
import Codec.Picture.Gif( encodeGifImages
                        , decodeGifImages
                        , GifLooping( LoopingForever )
                        , greyPalette )
import Foreign.Storable( Storable )

import Codec.Volume.Types

data ImageForVolume
  = Image8  (Image Pixel8)
  | Image16 (Image Pixel16)
  | ImageNotSuitable

toVolumeImage :: DynamicImage -> ImageForVolume
toVolumeImage d = case d of
  ImageY8 i -> Image8 i
  ImageYA8 i -> Image8 $ extractLumaPlane i
  ImageRGB8 i -> Image8 $ extractLumaPlane i
  ImageRGBA8 i -> Image8 $ extractLumaPlane i
  ImageYCbCr8 i -> Image8 $ extractLumaPlane i

  ImageY16  i -> Image16 $ extractLumaPlane i
  ImageYA16 i -> Image16 $ pixelMap (\(PixelYA16 y _) -> y) i
  ImageRGB16 i -> Image16 $ extractLumaPlane i
  ImageRGBA16 i -> Image16 $ pixelMap toLum i
    where toLum (PixelRGBA16 r g b _) = fromIntegral $ (fi r + fi g + fi b) `div` 3
          fi :: Word16 -> Int
          fi = fromIntegral

  ImageRGBF _ -> ImageNotSuitable
  ImageYF _ -> ImageNotSuitable
  ImageCMYK8 _ -> ImageNotSuitable
  ImageCMYK16 _ -> ImageNotSuitable

is8 :: ImageForVolume -> Bool
is8 i = case i of
  Image8  _ -> True
  Image16 _ -> False
  ImageNotSuitable -> False

is16 :: ImageForVolume -> Bool
is16 i = case i of
  Image8  _ -> False
  Image16 _ -> True
  ImageNotSuitable -> False

get8 :: (Monoid (f (Image Pixel8)), Applicative f) => ImageForVolume -> f (Image Pixel8)
get8 (Image8 i) = pure i
get8 _ = mempty

get16 :: (Monoid (f (Image Pixel16)), Applicative f) => ImageForVolume -> f (Image Pixel16)
get16 (Image16 i) = pure i
get16 _ = mempty

imageSizeIdenticals :: Image a -> Image a -> Bool
imageSizeIdenticals a b =
  imageWidth a == imageWidth b && imageHeight a == imageHeight b

-- | Convert a list of images to a volume.
--
-- All images must be of the same size.
volumeOfImages :: (Storable a, PixelBaseComponent a ~ a)
               => [Image a] -> Either String (Volume a)
volumeOfImages [] = Left "No images provided"
volumeOfImages lst@(i:rest)
  | not (all (imageSizeIdenticals i) rest) = Left "Different image sizes"
  | otherwise = Right $ Volume
     { _volumeSize  = VolumeSize (imageWidth i) (imageHeight i) (length lst)
     , _volumeVoxel = VoxelSize 1 1 1
     , _volumeData  = VSM.concat $ fmap imageData lst
     }

-- | Load a list of images to a volume. The depth will be the number
-- of images.
--
-- All images must be of the same dimension and have the same
-- bit depth.
volumeFromImageFiles :: [FilePath] -> IO (Either String DynamicVolume)
volumeFromImageFiles pathes = do
  mayImages <- sequence <$> mapM readImage pathes
  return $ mayImages >>= volumeFromDynamicImages

-- | Convert a list of loaded dynamic images to a volume.
-- All images must be of the same dimension and have the same
-- bit depth.
volumeFromDynamicImages :: [DynamicImage] -> Either String DynamicVolume
volumeFromDynamicImages imgs =
  case (foldMap get8 images8, foldMap get16 images16, rest) of
    ( [],  [],  []) -> Left "No images provided"
    (_:_, _:_, _:_) -> Left "Mixed image types"
    ( [], _:_, _:_) -> Left "16bits and wrong color space for volume"
    (_:_,  [], _:_) -> Left "8bits and wrong color space for volume"
    (_:_, _:_,  []) -> Left "Mixed bit depth (16 & 8) images, can't create volum"
    ( [],  [], _:_) -> Left "Unusable images (wrong colorspace)"
    ( [], lst,  []) -> Volume16 <$> volumeOfImages lst
    (lst,  [],  []) -> Volume8 <$> volumeOfImages lst
  where
    (images8, i16AndRest) = partition is8 $ toVolumeImage <$> imgs
    (images16, rest) = partition is16 i16AndRest

-- | Encode a volume as a GIF image
encodeGifVolume :: Volume Word8 -> Either String LB.ByteString
encodeGifVolume vol = 
  encodeGifImages LoopingForever $ (greyPalette, 7,) <$> slices vol

-- | Write a volume as a GIF file.
writeGifVolume :: FilePath -> Volume Word8 -> Either String (IO ())
writeGifVolume fname volume = LB.writeFile fname <$> encodeGifVolume volume

-- | Decode a volume from an in-memory gif file.
decodeGifVolume :: B.ByteString -> Either String DynamicVolume
decodeGifVolume str = decodeGifImages str >>= volumeFromDynamicImages



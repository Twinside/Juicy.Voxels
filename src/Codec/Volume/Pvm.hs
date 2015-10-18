{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
-- | Module implementaing the PVM file reading.
module Codec.Volume.Pvm( readPVM, decodePVM ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (*>), (<$>) )
#endif

import Data.Bifunctor( bimap )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as VS

import Text.Read( readMaybe )
import Data.Binary( Binary( get ) )
import Data.Binary.Get( Get
                      , runGetOrFail
                      , getByteString )

import Codec.Volume.VectorByteConversion
import Codec.Volume.Types
import Codec.Volume.Dds
import Data.Binary.Ascii

-- | Decode a PVM (potentially with dds compression) in memory.
decodePVM :: B.ByteString -> Either String DynamicVolume
decodePVM = decodeDecompressed . decodeDDS 

-- | Read a pvm file potentially with DDS compression.
readPVM :: FilePath -> IO (Either String DynamicVolume)
readPVM fname = decodePVM <$> B.readFile fname

decodeDecompressed :: B.ByteString -> Either String DynamicVolume
decodeDecompressed str
  | pvmSig `B.isPrefixOf` str = go 1 . BL.fromStrict $ B.drop (B.length pvmSig) str
  | pvm2Sig `B.isPrefixOf` str = go 2 . BL.fromStrict $ B.drop (B.length pvm2Sig) str
  | pvm3Sig `B.isPrefixOf` str = go 3 . BL.fromStrict $ B.drop (B.length pvm3Sig) str
  | otherwise = Left "Unknown pvm signature"
  where
    pvmSig = "PVM\n"
    pvm2Sig = "PVM2\n"
    pvm3Sig = "PVM3\n"

    parser 1 = (, VoxelSize 1 1 1,) <$> getSizes <*> getBytePerSample
    parser 2 =  do
      !sizes <- getSizes 
      !voxSize <- getVoxelSize
      !bps <- getBytePerSample
      return (sizes, voxSize, bps)
    parser 3 = parser 2
    parser _ = fail "Unknown PVM version"

    third (_, _, v) = v

    go :: Int -> BL.ByteString -> Either String DynamicVolume
    go ver vstr = bimap third third . flip runGetOrFail vstr $ do
      (volumeSize, voxelSize, sizePerComp) <- parser ver
      let sampCount = samplesInVolume volumeSize
          toVolume = Volume volumeSize voxelSize
      samples <- getByteString $ sampCount * sizePerComp
      case sizePerComp of
        1 -> pure . Volume8 . toVolume $ byteStringToVector samples 
        2 -> pure . Volume16 . toVolume . VS.map endianSwap $ byteStringToVector samples
        _ -> fail "Invlid volume size"

getSizes :: Get VolumeSize
getSizes = do
  BCD w <- get <* getSpaces
  BCD h <- get <* getSpaces
  BCD d <- get <* getSpaces
  NewLine <- get
  return $ VolumeSize w h d

getVoxelSize :: Get (VoxelSize Float)
getVoxelSize = do
  n1 <- eatWhile (/= ' ') <* getSpaces
  n2 <- eatWhile (/= ' ') <* getSpaces
  n3 <- eatWhile (`notElem` (" \r\n" :: String)) <* getSpaces
  NewLine <- get
  case (,,) <$> readMaybe n1 <*> readMaybe n2 <*> readMaybe n3 of
    Nothing -> fail "Invalid sizes"
    Just (sw, sh, sd) -> pure $ VoxelSize sw sh sd

getBytePerSample :: Get Int
getBytePerSample = do
  BCD bps <- get
  NewLine <- get
  pure bps


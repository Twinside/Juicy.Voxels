{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Codec.Volume.Pvm( decodeDDS, decodePVM ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (*>) )
#endif

import Control.Monad.Trans( lift )
import Control.Monad.ST( ST, runST )
import Control.Monad( foldM, forM_)
import Data.Bifunctor( bimap )
import Data.Bits( bit, unsafeShiftL )
import Data.Word( Word8, Word32 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Text.Read( readMaybe )
import Data.Binary( Binary( get ) )
import Data.Binary.Get( Get
                      , runGetOrFail
                      , getByteString )

import Codec.Volume.VectorByteConversion
import Codec.Volume.BitIO
import Codec.Volume.Types
import Codec.Volume.VectorOutput

import Data.Binary.Ascii

import Debug.Trace
import Text.Printf

ddsV1Sig, ddsV2Sig :: B.ByteString
ddsV1Sig = "DDS v3d\n"
ddsV2Sig = "DDS v3e\n"

ddsV1BlockSize, ddsV2BlockSize :: Maybe Int
ddsV1BlockSize = Nothing
ddsV2BlockSize = Just (1 `unsafeShiftL` 24)

readBits :: Int -> BoolReader s Word32
readBits = getNextBitsMSBFirst 

getStripSize :: BoolReader s Int
getStripSize = fromIntegral <$> readBits 7

readDDSBitSize :: BoolReader s Int
readDDSBitSize = do
  bits <- fromIntegral <$> readBits 3
  if bits >= 1 then
    pure $ bits + 1
  else
    pure bits
  
readSignedInt :: Int -> BoolReader s Int
{-# INLINE readSignedInt #-}
readSignedInt 0 = pure 0
readSignedInt bitCount = do
  value <- fromIntegral <$> readBits bitCount
  pure $! value - bit (bitCount - 1)

foldMi_ :: Monad m => Int -> acc -> (acc -> Int -> m acc) -> m acc
{-# INLINE foldMi_ #-}
foldMi_ maxi acc0 f = go 0 acc0 where
  go !ix !acc | ix >= maxi = pure acc
              | otherwise = f acc ix >>= go (ix + 1)


interleave :: VSM.Storable a => Int -> VS.Vector a -> VSM.STVector s a
           -> ST s (VSM.STVector s a)
interleave stride input output = do
  let len = VS.length input
      writer readIndex j = do
        VSM.unsafeWrite output j $ input `VS.unsafeIndex` readIndex
        pure $ readIndex + 1

  _ <- foldMi_ stride 0 $ \readIndex i ->
         foldM writer readIndex [i, i + stride .. len - 1 ]

  pure output

ddsInterleave :: Int -> Maybe Int -> VS.Vector Word8 -> VS.Vector Word8
ddsInterleave skip _ arr | skip <= 1 = arr
ddsInterleave skip Nothing arr = VS.create $ do
  out <- VSM.new (VS.length arr)
  _ <- interleave skip arr out
  pure out
ddsInterleave skip (Just blockSize) arr = VS.create $ do
  let maxi = VS.length arr
      blockByte = blockSize * skip
      blockCount = (maxi + blockSize - 1) `div` blockSize
  out <- VSM.new maxi
  forM_ [0 .. blockCount - 1] $ \ix -> do
    let input = VS.take blockByte $ VS.drop (ix * blockByte) arr
        output = VSM.take blockByte $ VSM.drop (ix * blockByte) out
    interleave skip input output
  pure out

decodeDDSPayload :: Maybe Int -> BoolReader s (VS.Vector Word8)
decodeDDSPayload blockSize = do
  skip <- (+ 1) . fromIntegral <$> readBits 2
  stripMax <- (+ 1) . fromIntegral <$> readBits 16
  vecOut <-
        trace (printf "skip:%d stripMax: %d" skip stripMax) $ 
        lift newVectorOut
  let stripRead _acc' _written 0 = pure ()
      stripRead acc' writtenTop stripSize = do
        bitCount <- readDDSBitSize
        let go !currStrip !written !acc | currStrip >= stripSize = do
              getStripSize >>= stripRead acc written
            go currStrip written acc = do
              !val <- readSignedInt bitCount
              !accNext <-
                if written > stripMax && stripMax > 1 then lift $ do
                  !p0 <- unsafePrevValueAt vecOut stripMax
                  !p1 <- unsafePrevValueAt vecOut (stripMax + 1)
                  pure $! acc + val + fromIntegral p0 - fromIntegral p1
                else
                  pure $! acc + val
              let !finalVal = accNext  `mod` 256
              _ <- push vecOut $ fromIntegral finalVal
              go (currStrip + 1) (written + 1) finalVal

        go 0 writtenTop acc'

  getStripSize >>= stripRead 0 0
  lift $ ddsInterleave skip blockSize <$> finalizeOutput vecOut

decodePVM :: B.ByteString -> Either String DynamicVolume
decodePVM = decodeDecompressed . (\s -> trace (show $ B.take 200 s) s) . decodeDDS 

decodeDDS :: B.ByteString -> B.ByteString
decodeDDS str 
  | ddsV1Sig `B.isPrefixOf` str = go ddsV1Sig ddsV1BlockSize
  | ddsV2Sig `B.isPrefixOf` str = go ddsV2Sig ddsV2BlockSize
  | otherwise = str 
  where
    go sig blockSize = runST $ runBoolReader $ do
      setDecodedString $ B.drop (B.length sig) str
      toByteString <$> decodeDDSPayload blockSize


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


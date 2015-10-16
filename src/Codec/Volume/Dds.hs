{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Codec.Volume.Dds
    ( decodeDDS
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (*>) )
#endif

import Control.Monad.Trans( lift )
import Control.Monad.ST( ST, runST )
import Control.Monad( foldM, forM_)
import Data.Bits( bit, unsafeShiftL )
import Data.Word( Word8, Word32 )
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM


import Codec.Volume.VectorByteConversion
import Codec.Volume.BitIO
import Codec.Volume.VectorOutput

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


decodeDDS :: B.ByteString -> B.ByteString
decodeDDS str 
  | ddsV1Sig `B.isPrefixOf` str = go ddsV1Sig ddsV1BlockSize
  | ddsV2Sig `B.isPrefixOf` str = go ddsV2Sig ddsV2BlockSize
  | otherwise = str 
  where
    go sig blockSize = runST $ runBoolReader $ do
      setDecodedStringMSB $ B.drop (B.length sig) str
      toByteString <$> decodeDDSPayload blockSize


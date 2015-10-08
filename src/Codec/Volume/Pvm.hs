{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codec.Volume.Pvm where

import Control.Monad.Trans( lift )
import Control.Monad.ST( runST )
import Control.Applicative( (*>) )
import Data.Bits( bit )
import Data.Word( Word8, Word32 )
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS

import Data.Binary( Binary( get ) )
import Data.Binary.Get( Get, lookAhead )

import Codec.Volume.VectorByteConversion
import Codec.Volume.BitIO
import Codec.Volume.VectorOutput
import Data.BinaryCodedDigits

import Debug.Trace
import Text.Printf

ddsV1Sig, ddsV2Sig :: B.ByteString
ddsV1Sig = "DDS v3d\n"
ddsV2Sig = "DDS v3e\n"

type IntVal = Int

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
readSignedInt 0 = pure 0
readSignedInt bitCount = do
  value <- fromIntegral <$> readBits bitCount
  pure $ value - bit (bitCount - 1)

foldMi_ :: Monad m => Int -> acc -> (acc -> Int -> m acc) -> m acc
foldMi_ maxi acc0 f = go 0 acc0 where
  go ix acc | ix >= maxi = pure acc
            | otherwise = f acc ix >>= go (ix + 1)

decodeDDSPayload :: BoolReader s (VS.Vector Word8)
decodeDDSPayload = do
  componentCount :: Int <- (+ 1) . fromIntegral <$> readBits 2
  stripMax <- (+ 1) . fromIntegral <$> readBits 16
  vecOut <- trace (printf "skip:%d stripMax: %d" componentCount stripMax) $ lift newVectorOut
  let stripRead acc _written 0 = pure acc
      stripRead acc written stripSize = do
        bitCount <- readDDSBitSize
        (written', acc') <- trace (printf "stripSize: %d bitCount: %d" stripSize bitCount)$
          foldMi_ stripSize (written, acc) $ \(!acc, !written) _ -> do
            val <- readSignedInt bitCount
            acc' <-
              if written > stripMax && stripMax > 1 then do
                p0 <- lift $ prevValueAt vecOut stripMax 0
                p1 <- lift $ prevValueAt vecOut (stripMax + 1) 0
                pure $ acc + val + fromIntegral p0 - fromIntegral p1
              else
                pure $ acc + val
            let finalVal = acc' `mod` 256
            push vecOut $ fromIntegral finalVal
            pure (finalVal, written + 1)

        getStripSize >>= stripRead acc' written'

  getStripSize >>= stripRead 0 0
  lift $ finalizeOutput vecOut

decodeDDS :: B.ByteString -> B.ByteString
decodeDDS str 
  | ddsV1Sig `B.isPrefixOf` str = runST $ runBoolReader $ do
        setDecodedString $ B.drop (B.length ddsV1Sig) str
        toByteString <$> decodeDDSPayload
  | otherwise = str 

getSizes :: Get (Int, Int, Int)
getSizes = do
  BCD w <- get <* getSpaces
  BCD h <- get <* getSpaces
  BCD d <- get <* getSpaces
  NewLine <- get
  return (w, h, d)


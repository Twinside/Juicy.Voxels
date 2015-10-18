{-# LANGUAGE TupleSections #-}
import Data.Word( Word8 )
import Codec.Volume
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString.Char8 as B

toGif :: FilePath -> Volume Word8 -> IO ()
toGif fname vol = case writeGifVolume fname vol of
    Left err -> putStrLn $ "Error : " ++ err
    Right v -> v

main :: IO ()
main = do
  {-file <- B.readFile "D:/Users/Vince/Downloads/Daisy_192x180x168_8bit.pvm"-}
  {-file <- B.readFile "Daisy.pvm"-}
  {-file <- B.readFile "CT-Head.pvm"-}
  file <- B.readFile "Carp.pvm"
  case decodePVM file of
    Left err -> putStrLn $ "ERROR : " ++ err
    Right (Volume8 vol) -> do
      toGif "anim.gif" vol
    Right (Volume16 vol) -> do
      let maxi = VS.maximum $ _volumeData vol
          scaler = 1 / (fromIntegral (maxi + 1) / 255.0)
      putStrLn $ "Maximum " ++ show maxi
      toGif "anim.gif" $ volumeMap (\a -> min 255 . floor $ (fromIntegral a :: Float) * scaler) vol


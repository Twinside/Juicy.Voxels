{-# LANGUAGE TupleSections #-}
import Data.Foldable( forM_ )
import Data.Word( Word8 )
import Codec.Volume
import Codec.Volume.Pvm
import Codec.Volume.VectorByteConversion
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString.Char8 as B
import Codec.Picture
import Codec.Picture.Gif
import Text.Printf

toImg :: Int -> Int -> Int -> VS.Vector Word8 -> Image Pixel8
toImg w h slice = Image w h . VS.take imgSize . VS.drop (imgSize * slice)
  where
    imgSize = w * h

toGif :: FilePath -> Volume Word8 -> IO ()
toGif fname vol = case rez of
    Left err -> putStrLn $ "Error : " ++ err
    Right v -> v
  where
    rez = writeGifImages fname LoopingForever $ (greyPalette, 7,) <$> slices vol

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


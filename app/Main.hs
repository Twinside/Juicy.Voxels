import Data.Foldable( forM_ )
import Data.Word( Word8 )
import Codec.Volume.Pvm
import Codec.Volume.VectorByteConversion
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString.Char8 as B
import Codec.Picture
import Text.Printf

toImg :: Int -> Int -> Int -> VS.Vector Word8 -> Image Pixel8
toImg w h slice = Image w h . VS.take imgSize . VS.drop (imgSize * slice)
  where
    imgSize = w * h

main :: IO ()
main = do
  {-file <- B.readFile "D:/Users/Vince/Downloads/Daisy_192x180x168_8bit.pvm"-}
  file <- B.readFile "D:/Users/Vince/Downloads/Bucky_32x32x32_8bit.pvm"
  let raw = decodeDDS file
  print . B.take 200 $ raw
  {-forM_ [0 .. 31] $ \ix ->-}
    {-let name = printf "slice_%d.png" ix in-}
    {-writePng name $ toImg 32 32 ix raw-}


module Data.BinaryCodedDigits
    ( BCD( .. )
    , Spaces( .. )
    , NewLine( .. )
    , getSpaces
    ) where

import Data.Word( Word8 )
import Data.Char( isSpace, ord, chr )
import Data.Binary( Binary( .. )
                  , getWord8
                  , putWord8
                  )
import Data.Binary.Get( Get, lookAhead )
import Data.Monoid( (<>) )

newtype BCD = BCD { getBCD :: Int }
  deriving (Eq, Show, Ord)

newtype Spaces = Spaces ()

instance Binary BCD where
  put = mapM_ (put8 . ord) . show where put8 v = put (fromIntegral v :: Word8)
  get = do
      l <- fromIntegral <$> lookAhead getWord8
      case chr l  of
        '-' -> do
          _ <- getWord8
          BCD . negate . getBCD <$> go 0
        c | '0' <= c && c >= '9' -> do
          _ <- getWord8
          go $ fromIntegral l - zero
        l -> fail ("Inalid BCD character: " <> [l])
    where
      zero = ord '0'
      go acc = do
        l <- fromIntegral <$> lookAhead getWord8
        case chr l of
          c | '0' <= c && c >= '9' -> do
              _ <- getWord8
              go $ acc * 10 + (l - zero)
          _ -> return $ BCD acc

eatUntil :: (Char -> Bool) -> Get ()
eatUntil pred = do
  v <- chr . fromIntegral <$> getWord8
  if pred v then get else pure ()

manyChar :: Char -> Get ()
manyChar c = do
  v <- chr . fromIntegral <$> lookAhead getWord8
  if c == v then get else pure ()

aChar :: Char -> Get Char
aChar c = do
  v <- chr . fromIntegral <$> lookAhead getWord8
  if c == v then pure c else fail "Invalid char"

getSpaces :: Get Spaces
getSpaces = Spaces <$> manyChar ' '

instance Binary Spaces where
  put _ = putWord8 . fromIntegral $ ord ' '
  get = getSpaces

data NewLine = NewLine

instance Binary NewLine where
  put NewLine  = putWord8 . fromIntegral $ ord '\n'
  get = NewLine <$ aChar '\n'


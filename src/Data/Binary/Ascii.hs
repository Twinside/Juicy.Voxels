module Data.Binary.Ascii
    ( BCD( .. )
    , Spaces( .. )
    , NewLine( .. )
    , TextLine( .. )
    , getSpaces
    , eatWhile
    ) where

import Data.Word( Word8 )
import Data.Char( isSpace, ord, chr )
import Data.Binary( Binary( .. )
                  , getWord8
                  , putWord8
                  )
import Data.Binary.Get( Get, lookAhead )
import Data.Monoid( (<>) )

-- | Read an Int written in ASCII in a Binary
newtype BCD = BCD { getBCD :: Int }
  deriving (Eq, Show, Ord)

-- | Read many spaces
newtype Spaces = Spaces ()
  deriving (Eq, Show)

instance Binary BCD where
  put = mapM_ (put8 . ord) . show where put8 v = put (fromIntegral v :: Word8)
  get = do
      l <- fromIntegral <$> lookAhead getWord8
      case chr l  of
        '-' -> do
          _ <- getWord8
          BCD . negate . getBCD <$> go 0
        c | '0' <= c && c <= '9' -> do
          _ <- getWord8
          go $ fromIntegral l - zero
        l -> fail ("Inalid BCD character: '" <> [l] <> "'")
    where
      zero = ord '0'
      go acc = do
        l <- fromIntegral <$> lookAhead getWord8
        case chr l of
          c | '0' <= c && c <= '9' -> do
              _ <- getWord8
              go $ acc * 10 + (l - zero)
          _ -> return $ BCD acc

eatWhile :: (Char -> Bool) -> Get String
eatWhile pred = go where
  go = do
    c <- chr . fromIntegral <$> lookAhead getWord8
    if pred c then
        (c:) <$> (getWord8 *> go)
    else 
        pure []

manyChar :: Char -> Get ()
manyChar c = go where
  go = do
    v <- chr . fromIntegral <$> lookAhead getWord8
    if c == v then getWord8 *> go else pure ()

aChar :: Char -> Get Char
aChar c = do
  v <- chr . fromIntegral <$> lookAhead getWord8
  if c == v then 
    getWord8 *> pure c
  else
    fail "Invalid char"

nl :: Get ()
nl = do
  v <- chr . fromIntegral <$> lookAhead getWord8
  case v of
    '\r' -> getWord8 *> nl
    '\n' -> getWord8 *> pure ()
    _ -> fail "Invalid char"

getSpaces :: Get Spaces
getSpaces = Spaces <$> manyChar ' '

instance Binary Spaces where
  put _ = putWord8 . fromIntegral $ ord ' '
  get = getSpaces

-- | Read a newline character ('\r' or '\n' or "\r\n")
data NewLine = NewLine
  deriving (Eq, Ord, Show)

instance Binary NewLine where
  put NewLine  = putWord8 . fromIntegral $ ord '\n'
  get = NewLine <$ nl

data TextLine = TextLine { getTextLine :: String }
  deriving (Eq, Show, Ord)

eatLine :: Get String
eatLine = go where
  go = do
    v <- chr . fromIntegral <$> getWord8
    case v of
      '\n' -> pure []
      '\r' -> go
      c -> (c:) <$> go

instance Binary TextLine where
  get = TextLine <$> eatLine
  put (TextLine l) = do
    mapM_ (putWord8 . fromIntegral . ord) l
    put NewLine


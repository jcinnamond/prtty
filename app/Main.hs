module Main where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Char8 qualified as BIO
import Data.List qualified as L
import Data.String (IsString (..))

esc :: B.ByteString
esc = B.singleton 0x1b

newtype Attr = Attr {unAttr :: ByteString}

instance IsString Attr where
    fromString = Attr . BC.pack

instance Semigroup Attr where
    (Attr x) <> (Attr y) = Attr $ B.intercalate (BC.singleton ';') [x, y]

attrs :: [Attr] -> ByteString
attrs xs = esc <> BC.singleton '[' <> B.intercalate (BC.singleton ';') (unAttr <$> xs) <> BC.singleton 'm'

attr :: Attr -> ByteString
attr = attrs . L.singleton

reset :: Printable
reset = Style "0"

bold, strikethrough :: Attr
bold = "1"
strikethrough = "9"

black, red, green, yellow, blue :: Attr
black = "30"
red = "31"
green = "32"
yellow = "33"
blue = "34"

data Printable
    = Style Attr
    | Literal ByteString
    | Combined [Printable]

instance Semigroup Printable where
    (Combined xs) <> (Combined ys) = Combined (xs <> ys)
    (Combined xs) <> y = Combined (xs <> [y])
    x <> (Combined ys) = Combined (x : ys)
    x <> y = Combined [x, y]

instance IsString Printable where
    fromString = Literal . BC.pack

toByteString :: Printable -> ByteString
toByteString (Style (Attr x)) = esc <> BC.singleton '[' <> x <> BC.singleton 'm'
toByteString (Literal x) = x
toByteString (Combined xs) = B.concat (toByteString <$> xs)

output :: Printable -> IO ()
output = BIO.putStrLn . toByteString

main :: IO ()
main = do
    output $
        "hi "
            <> Style (red <> bold)
            <> "bold hi "
            <> reset
            <> Style blue
            <> "blue"

{-# LANGUAGE OverloadedStrings #-}
module Crypto.Util.Charset where

import Data.Char
import qualified Data.Text as T
import Data.Word

asciiPrintable :: T.Text
asciiPrintable = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n\r\x0b\x0c"

asciiLower :: T.Text
asciiLower = "abcdefghijklmnopqrstuvwxyz"

asciiUpper :: T.Text
asciiUpper = T.toUpper asciiLower

asciiAlpha :: T.Text
asciiAlpha = T.append asciiLower asciiUpper

asciiNum :: T.Text
asciiNum = "0123456789"

asciiAlnum :: T.Text
asciiAlnum = T.append asciiAlpha asciiNum

flagChars :: T.Text
flagChars = T.append asciiAlnum "!@#$%^&*.,-_? "

toW8List :: T.Text -> [Word8]
toW8List txt = map (fromIntegral . ord) (T.unpack txt)

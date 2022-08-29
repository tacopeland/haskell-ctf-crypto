# CRYPTOHACK CHALLENGES
This contains the solutions for many cryptohack challenges in order, and I
will update this file as the way I solve challenges changes (I create new
utility functions, etc.)


## General

### Encoding

#### ASCII

Convert `[99, 114, 121, 112, 116, 111, 123, 65, 83, 67, 73, 73, 95, 112, 114, 49, 110, 116, 52, 98, 108, 51, 125]` to a string.

```haskell
import CTF.Prelude
map chr [99, 114, 121, 112, 116, 111, 123, 65, 83, 67, 73, 73, 95, 112, 114, 49, 110, 116, 52, 98, 108, 51, 125]
```

#### Hex

Convert `63727970746f7b596f755f77696c6c5f62655f776f726b696e675f776974685f6865785f737472696e67735f615f6c6f747d` to bytes.

```haskell
import CTF.Prelude
hexToBytes (tPack "63727970746f7b596f755f77696c6c5f62655f776f726b696e675f776974685f6865785f737472696e67735f615f6c6f747d")
```

#### Base64

Convert `72bca9b68fc16ac7beeb8f849dca1d8a783e8acf9679bf9269f7bf` from hex to base64.

```haskell
import CTF.Prelude
(b64Enc . hexToBytes . tPack) "72bca9b68fc16ac7beeb8f849dca1d8a783e8acf9679bf9269f7bf"
```

#### Bytes and Big Integers

Convert `11515195063862318899931685488813747395775516287289682636499965282714637259206269` back into a message (little-endian).

```haskell
import CTF.Prelude
integerToText 11515195063862318899931685488813747395775516287289682636499965282714637259206269
```

#### Encoding Challenge

Pass all 100 levels of `nc socket.cryptohack.org 13377` to get the flag.

```haskell
{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

import CTF.Prelude
import CTF.Network.NetHelper
import Crypto.Cipher.Monoalphabetic (rot)

import Data.Aeson hiding (Error)
import Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B
import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll)
import System.IO

-- JSON data types
data Response =
    SEncoding { _stype :: String
              , _sencoded :: String }
  | IAEncoding { _itype :: String
               , _iencoded :: [Integer] }
  | Flag { __flag :: String }
  | Error { __error :: String }
  deriving (Read, Show)

data Encoded =
    Str String
  | IntArr [Integer]

newtype Msg =
    Msg { _decoded :: String }

-- Derive JSON represenations without tags
$(deriveJSON defaultOptions { fieldLabelModifier = drop 2, sumEncoding = UntaggedValue } ''Response)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 1, sumEncoding = UntaggedValue } ''Msg)

-- Encoding functions by name
dispatchEncode :: String -> Encoded -> String
dispatchEncode "base64" (Str enc) = B.toString (b64Dec (B.fromString enc))
dispatchEncode "hex" (Str enc) = B.toString (hexToBytes (tPack enc))
dispatchEncode "rot13" (Str enc) = tUnpack (rot 13 (tPack enc))
dispatchEncode "bigint" (Str enc) = tUnpack (integerToText (read enc :: Integer))
dispatchEncode "utf-8" (IntArr ints) = map (chr . fromInteger) ints
dispatchEncode _ _ = undefined

-- Recieve a line, process it, then choose whether to continue
handleLines :: Socket -> IO ()
handleLines sock = do
    input <- sRecvLine sock
    case eitherDecodeStrict input of
      Left err -> putStrLn err
      Right r -> do
          print r
          case r of
            SEncoding t e -> do
                sendAll sock (encode (Msg (dispatchEncode t (Str e))))
                handleLines sock
            IAEncoding t e -> do
                sendAll sock (encode (Msg (dispatchEncode t (IntArr e))))
                handleLines sock
            Flag f -> putStrLn f
            Error e -> putStrLn e


main :: IO ()
main = do
    withNC "socket.cryptohack.org" 13377 handleLines

```


### XOR

#### XOR Starter

XOR the string `"label"` with the integer `13`.

```haskell
import CTF.Prelude
strXor "label" "\x0d"
```

#### XOR Properties

Solve for flag:
```
KEY1 = a6c8b6733c9b22de7bc0253266a3867df55acde8635e19c73313
KEY2 ^ KEY1 = 37dcb292030faa90d07eec17e3b1c6d8daf94c35d4c9191a5e1e
KEY2 ^ KEY3 = c1545756687e7573db23aa1c3452a098b71a7fbf0fddddde5fc1
FLAG ^ KEY1 ^ KEY3 ^ KEY2 = 04ee9855208a2cd59091d04767ae47963170d1660df7f56f5faf 
```

Answer:
```haskell
import CTF.Prelude

k1 = hexToBytes (tPack "a6c8b6733c9b22de7bc0253266a3867df55acde8635e19c73313")
k2k3 = hexToBytes (tPack "c1545756687e7573db23aa1c3452a098b71a7fbf0fddddde5fc1")
flagk1k2k3 = hexToBytes (tPack "04ee9855208a2cd59091d04767ae47963170d1660df7f56f5faf")
flag = k1 `xor` k2k3 `xor` flagk1k2k3
```

#### Favourite byte

Single byte xor, decrypt `73626960647f6b206821204f21254f7d694f7624662065622127234f726927756d`

```haskell
import CTF.Prelude
import Crypto.Cipher.Xor
import Crypto.Util.Charset

ct = hexToBytes (tPack "73626960647f6b206821204f21254f7d694f7624662065622127234f726927756d")
bruteForceXor ct (toW8List flagChars) 1 (bPack "crypto{")
```

#### You either know, XOR you don't

Xor with a multi-byte key, decrypt `0e0b213f26041e480b26217f27342e175d0e070a3c5b103e2526217f27342e175d0e077e263451150105`

```haskell
import CTF.Prelude

ct = hexToBytes (tPack "0e0b213f26041e480b26217f27342e175d0e070a3c5b103e2526217f27342e175d0e077e263451150104")
len = guessKeyLength ct 40
bruteForceXorSW ct (toW8List flagChars) (fromIntegral len) (bPack "crypto{") (bPack "crypto{")
```
After this, just look for whichever key/pt pair works best.

#### Lemur XOR


### Mathematics
#### Greatest Common Divisor
#### Extended GCD
#### Modular Arithmetic 1
#### Modular Arithmetic 2
#### Modular Inverting

### Data Formats
#### Privacy-Enhanced Mail?
#### CERTainly not
#### SSH Keys
#### Transparency

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
import CTF.Prelude
import CTF.Network.NetHelper

import System.IO

data Response =
    Encoding {
      type :: String
      encoded :: String }
  | Flag {
      flag :: String }
  | Error {
      error :: String }


handleLine :: Handle -> IO ()
handleLine h =
  in <- hGetLine h

  

withNC "socket.cryptohack.org 13377" $ \h -> do
  
```


### XOR

#### XOR Starter
#### XOR Properties
#### Favourite byte
#### You either know, XOR you don't
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

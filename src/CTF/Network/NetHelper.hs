module CTF.Network.NetHelper where

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as B
import Data.Functor ((<&>))
import Network.Socket
  ( close, connect, socket, defaultProtocol, getAddrInfo, socketToHandle, addrFamily
  , addrAddress, Socket(..), SocketType(..))
import Network.Socket.ByteString (recv, sendAll)

import System.IO


withNC :: String -> Int -> (Socket -> IO a) -> IO ()
withNC hostname port fun = do
    addrInfo <- getAddrInfo Nothing (Just hostname) (Just (show port))
    case addrInfo of
      (serverAddr:_) -> do
          sock <- socket (addrFamily serverAddr) Stream defaultProtocol
          connect sock (addrAddress serverAddr)
          _ <- fun sock
          close sock
      _ -> hPutStrLn stderr ("Could not resolve host " ++ hostname)

sRecvLine :: Socket -> IO ByteString
sRecvLine s = do
    c <- recv s 1
    if B.null c
       then return c
       else case B.head c of
              0x0a -> return B.empty
              ch   -> sRecvLine s <&> B.cons ch

sRecvUntil :: Socket -> ByteString -> IO ByteString
sRecvUntil s t = inner B.empty B.empty
  where tailLen = B.length t
        inner res last = do
          c <- recv s 1
          let newRes = B.snoc res (B.head c)
              newLast = if B.length last < tailLen
                           then newRes
                           else B.snoc (B.drop 1 last) (B.head c)
          if B.null c
             then return res
             else if newLast == t
                    then return newRes
                    else inner newRes newLast
                    

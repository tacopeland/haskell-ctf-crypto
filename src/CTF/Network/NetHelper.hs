module CTF.Network.NetHelper where

import qualified Data.ByteString as BS
import Network.Socket
  ( connect, socket, defaultProtocol, getAddrInfo, socketToHandle, addrFamily
  , addrAddress, SocketType(..))
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hClose, hPutStrLn, stderr, Handle(..), IOMode(..))


withNC :: String -> Int -> (Handle -> IO a) -> IO ()
withNC hostname port fun = do
    addrInfo <- getAddrInfo Nothing (Just hostname) (Just (show port))
    case addrInfo of
      (serverAddr:_) -> do
          sock <- socket (addrFamily serverAddr) Stream defaultProtocol
          connect sock (addrAddress serverAddr)
          h <- socketToHandle sock ReadWriteMode
          _ <- fun h
          hClose h
      _ -> hPutStrLn stderr ("Could not resolve host " ++ hostname)

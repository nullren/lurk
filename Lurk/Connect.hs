module Lurk.Connect where

import Crypto.Random
import Lurk.Types
import Network
import Network.TLS
import Network.TLS.Extra
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.IO

connect_ :: Bool -> String -> Int -> IO ConnInfo
connect_ ssl = if ssl then connect_ssl else connect_reg

-- | Make a SSL connection to a host
connect_ssl :: String -> Int -> IO ConnInfo
connect_ssl host port = do
  gen <- newGenIO :: IO SystemRandom
  ctx <- connectionClient host (show port) params gen
  E.catch (handshake ctx) (\(E.SomeException e) -> putStrLn $ show e)
  return ConnInfo
    { connRead = recvData ctx
    , connWrite = sendData ctx
    , connClose = bye ctx
    }
  where
    params = defaultParamsClient {pCiphers = ciphersuite_all}

-- | Make a plaintext connection to a host
connect_reg :: String -> Int -> IO ConnInfo
connect_reg host port = do
  h <- connectTo host (PortNumber . fromIntegral $ port)
  hSetBuffering h NoBuffering
  return ConnInfo
    { connRead = B.hGetLine h
    , connWrite = L.hPutStr h
    , connClose = hClose h
    }

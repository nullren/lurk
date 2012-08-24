module Lurk.Connect where

import Control.Applicative
import Control.Monad
import Crypto.Random
import Lurk.Types
import Network
import Network.TLS
import Network.TLS.Extra
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO

-- | Connect to host, tell whether SSL or not.
connect_ :: Bool -> String -> Int -> IO ConnInfo
connect_ ssl = if ssl then connectSsl else connectReg

-- | Make a SSL connection to a host.
connectSsl :: String -> Int -> IO ConnInfo
connectSsl host port = do
  gen <- newGenIO :: IO SystemRandom
  ctx <- connectionClient host (show port) params gen
  E.catch (handshake ctx) (\(E.SomeException e) -> print e)
  return ConnInfo
    { connRead = recvData ctx
    , connWrite = sendData ctx
    , connClose = bye ctx
    }
  where
    params = defaultParamsClient {pCiphers = ciphersuite_all}

-- | Make a plaintext connection to a host.
connectReg :: String -> Int -> IO ConnInfo
connectReg host port = do
  h <- connectTo host (PortNumber . fromIntegral $ port)
  hSetBuffering h NoBuffering
  return ConnInfo
    { connRead = (\x -> B.concat [x, B.pack "\n"]) <$> (B.hGetLine h)
    , connWrite = L.hPutStr h
    , connClose = hClose h
    }

import Control.Concurrent
import Control.Monad
import Crypto.Random
import Data.List
import Network.TLS
import Network.TLS.Extra
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO

host = "irc.oftc.net"
port = "6697"

params = defaultParamsClient {pCiphers = ciphersuite_all}

-- | Makes a SSL connection to the server.
connect_ssl :: IO ()
connect_ssl = do
  gen <- newGenIO :: IO SystemRandom
  ctx <- connectionClient host port params gen
  E.catch (handshake ctx) (\(E.SomeException e) -> putStrLn $ show e)

  forkIO $ forever (getLine >>= write_ssl ctx)

  write_ssl ctx "NICK testbot"
  write_ssl ctx "USER testbot 0 * :derpington"
  listen_ssl ctx $ L.pack ""

listen_ssl :: TLSCtx -> L.ByteString -> IO ()
listen_ssl ctx bs = do
  if (L.count '\n' bs) > 0
    then do
      let s' = L.takeWhile (not . (=='\r')) bs
      let s  = L.unpack s'
      putStrLn s
      if ping s then pong s else handle s
      listen_ssl ctx $ L.drop (2 + L.length s') bs
    else do
      out <- recvData ctx
      listen_ssl ctx $ L.concat [bs,L.pack $ B.unpack out]
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write_ssl ctx $ "PONG :" ++ (drop 6 x)

write_ssl :: TLSCtx -> String -> IO ()
write_ssl c s = do
  sendData c $ L.pack (s ++ "\r\n")
  putStrLn $ "> " ++ s

handle :: String -> IO ()
handle s = do
    return ()
  where
    clean = drop 1 . dropWhile (not . (==':')) . drop 1 

main :: IO ()
main = connect_ssl

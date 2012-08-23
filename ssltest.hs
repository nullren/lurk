import System.IO
import Network.TLS
import Network.TLS.Extra
import Crypto.Random
import Text.Printf
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import Control.Monad

host = "irc.freenode.net"
port = "6697"

params = defaultParams {pCiphers = ciphersuite_all}

-- | Makes a SSL connection to the server.
connect_ssl :: IO ()
connect_ssl = do
  gen <- newGenIO :: IO SystemRandom
  ctx <- connectionClient host port params gen
  E.catch (handshake ctx) (\(E.SomeException e) -> putStrLn $ show e)
  let setnick = L.pack "NICK testderp\r\nUSER testderp 0 * :Testing the derp\r\n"
  sendData ctx setnick
  listen ctx

listen :: TLSCtx a -> IO ()
listen ctx = forever $ do
  out <- recvData ctx
  {-if B.isSuffixOf . L.pack "\r\n" $ out
    then putStrLn "one line"
    else putStrLn "more lines"-}
  B.putStr out

main :: IO ()
main = connect_ssl

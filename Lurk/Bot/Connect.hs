module Lurk.Bot.Connect where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Crypto.Random
import Data.List
import Lurk.Bot.Config
import Lurk.Handler
import Lurk.Logger
import Network
import Network.TLS
import Network.TLS.Extra
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO
import System.Time
import Text.Printf



connect :: BotConfig -> IO Bot
connect cfg = notify $ do
  t <- getClockTime
  h <- connectTo (server cfg) (PortNumber . fromIntegral . port $ cfg)
  db <- connectLog cfg
  forkIO $ forever $ getLine >>= hPrintf h "%s\r\n"
  hSetBuffering h NoBuffering
  return (Bot h t cfg db)
  where
    notify a = E.bracket_
      (printf "Connecting to %s ... " (server cfg) >> hFlush stdout)
      (putStrLn "done.")
      a

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

listen :: (String -> Net ()) -> Handle -> Net ()
listen hndr h = forever $ do
  s <- init `fmap` liftIO (hGetLine h)
  liftIO $ putStrLn s
  if ping s then pong s else hndr s
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

listen_ssl :: (String -> Net ()) -> TLSCtx -> L.ByteString -> Net ()
listen_ssl hndr ctx bs = do
  if (L.count '\n' bs) > 0
    then do
      let s' = L.takeWhile (not . (=='\r')) bs
      let s  = L.unpack s'
      liftIO $ putStrLn s
      if ping s then pong s else hndr s
      listen_ssl hndr ctx $ L.drop (2 + L.length s') bs
    else do
      out <- liftIO $ recvData ctx
      listen_ssl hndr ctx $ L.concat [bs,L.pack $ B.unpack out]
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write_ssl ctx $ "PONG :" ++ (drop 6 x)

write_ssl :: TLSCtx -> String -> Net ()
write_ssl c s = do
  liftIO $ sendData c $ L.pack (s ++ "\r\n")
  liftIO $ putStrLn $ "> " ++ s

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  liftIO $ hPrintf h "%s %s\r\n" s t
  liftIO $ printf    "> %s %s\n" s t


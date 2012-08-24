module Lurk.Bot
  ( runBot
  , module Lurk.Types
  ) where

import Data.List
import Network
import System.IO
import System.Time
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Control.Exception as E
import Text.Printf
import Lurk.Types
import Lurk.Connect
import Lurk.Bot.IRC
import Lurk.Logger
import Lurk.Handler
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B


runBot :: BotConfig -> IO ()
runBot cfg = E.bracket (connect cfg) disconnect loop
  where
    disconnect = connClose . connInfo
    loop s = E.catch (runReaderT run s) (\(E.SomeException e) -> putStrLn $ show e)

connect :: BotConfig -> IO Bot
connect cfg = notify $ do
  t <- getClockTime
  conn <- connect_ (ssl cfg) (server cfg) (port cfg)
  db <- connectLog cfg
  forkIO $ forever $ getLine >>= writer conn
  return (Bot conn t cfg db)
  where
    writer c s = (connWrite c) $ L.pack $ s ++ "\r\n"
    notify a = E.bracket_
      (printf "Connecting to %s ... " (server cfg) >> hFlush stdout)
      (putStrLn "done.")
      a

run :: Net ()
run = do
  cfg <- asks config
  write "NICK" (nick cfg)
  write "USER" ((username cfg)++" 0 * :" ++ (realname cfg))
  asks connInfo >>= listen

listen :: ConnInfo -> Net ()
listen h = forever $ do
  conn <- asks connInfo
  s' <- liftIO $ connRead conn
  let s = B.unpack s'
  liftIO $ putStrLn s
  if ping s then pong s else handle s
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

module Lurk.Bot
  ( runBot
  , module Lurk.Types
  , module Lurk.Handlers
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.List
import Lurk.Bot.IRC
import Lurk.Connect
import Lurk.Handler
import Lurk.Handlers
import Lurk.Logger
import Lurk.Types
import Network
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO
import System.Time
import Text.Printf

-- | Start the bot by giving it configuration holding all the details of
-- of the connection and session.
runBot :: BotConfig -> IO ()
runBot cfg = E.bracket (connect cfg) disconnect loop
  where
    disconnect = connClose . connInfo
    loop s = E.catch (runReaderT run s) (\(E.SomeException e) -> print e)

connect :: BotConfig -> IO Bot
connect cfg = notify $ do
  t <- getClockTime
  conn <- connect_ (ssl cfg) (server cfg) (port cfg)
  db <- connectLog cfg
  forkIO $ forever $ getLine >>= writer conn
  return (Bot conn t cfg db)
  where
    writer c s = connWrite c $ L.pack $ s ++ "\r\n"
    notify = E.bracket_
      (printf "Connecting to %s ... " (server cfg) >> hFlush stdout)
      (putStrLn "done.")

run :: Net ()
run = do
  cfg <- asks config
  write "NICK" (nick cfg)
  write "USER" (username cfg ++ " 0 * :" ++ realname cfg)
  asks connInfo >>= listen

listen :: ConnInfo -> Net ()
listen h = listen' h $ L.pack ""

listen' :: ConnInfo -> L.ByteString -> Net ()
listen' conn bs = if L.count '\n' bs > 0 then 
    do let s' = L.takeWhile (not . (=='\r')) bs
       let s  = L.unpack s'
       liftIO $ putStrLn s
       if ping s then pong s else handle s
       listen' conn $ L.drop (2 + L.length s') bs
    else
    do out <- liftIO $ connRead conn
       listen' conn $ L.concat [bs,L.pack $ B.unpack out]
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)


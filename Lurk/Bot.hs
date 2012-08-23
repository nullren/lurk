module Lurk.Bot where

import Data.List
import Network
import System.IO
import System.Time
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Control.Exception as E
import Text.Printf
import Lurk.Bot.Config
import Lurk.Bot.IRC
import Lurk.Handler

runBot :: BotConfig -> IO ()
runBot cfg = E.bracket (connect cfg) disconnect loop
  where
    disconnect = hClose . socket
    loop s = E.catch (runReaderT run s) (\(E.SomeException e) -> putStrLn $ show e)

connect :: BotConfig -> IO Bot
connect cfg = notify $ do
  t <- getClockTime
  h <- connectTo (server cfg) (PortNumber . fromIntegral . port $ cfg)
  forkIO $ forever $ getLine >>= hPrintf h "%s\r\n"
  hSetBuffering h NoBuffering
  return (Bot h t cfg)
  where
    notify a = E.bracket_
      (printf "Connecting to %s ... " (server cfg) >> hFlush stdout)
      (putStrLn "done.")
      a

run :: Net ()
run = do
  cfg <- asks config
  write "NICK" (nick cfg)
  write "USER" ((username cfg)++" 0 * :" ++ (realname cfg))
  asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
  s <- init `fmap` liftIO (hGetLine h)
  liftIO $ putStrLn s
  if ping s then pong s else handle s
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

module Lurk.Bot.IRC where

import Control.Monad.Reader
import Control.Monad
import Text.Printf
import Lurk.Bot.Config

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  liftIO $ hPrintf h "%s %s\r\n" s t
  liftIO $ printf    "> %s %s\n" s t

privmsg :: String -> String -> Net ()
privmsg c s = write "PRIVMSG" (c ++ " :" ++ s)


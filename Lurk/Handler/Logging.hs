module Lurk.Handler.Logging (logHandler) where

import Lurk.Bot.IRC
import Lurk.Bot.Config
import Lurk.Logger
import Control.Monad.Reader
import Network.IRC hiding (nick, privmsg)
import Data.List

logHandler :: Maybe Message -> Net ()
logHandler msg = case msg of
  Nothing -> return ()
  Just m -> writeLog $ encode m

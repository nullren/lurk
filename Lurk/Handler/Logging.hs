module Lurk.Handler.Logging (logHandler) where

import Control.Monad.Reader
import Data.List
import Lurk.Bot.IRC
import Lurk.Logger
import Lurk.Types
import Network.IRC hiding (nick, privmsg)

-- | Log everything.
logHandler :: Maybe Message -> Net ()
logHandler msg = case msg of
  Nothing -> return ()
  Just m -> writeLog $ encode m

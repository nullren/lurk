module Lurk.Handler.Logging (logHandler) where

import Lurk.Bot.IRC
import Lurk.Bot.Config
import Control.Monad.Reader
import Network.IRC hiding (nick, privmsg)
import Data.List

logHandler msg = do
  cfg <- asks config
  sql <- asks db
  case sql of
    Nothing -> return ()
    Just db -> do
      run db "insert into rawlog values (date('now'), ?)" [toSql $ encode msg]
      commit db

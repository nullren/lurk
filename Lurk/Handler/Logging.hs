module Lurk.Handler.Logging (logHandler) where

import Lurk.Bot.IRC
import Lurk.Bot.Config
import Control.Monad.Reader
import Network.IRC hiding (nick, privmsg)
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3

logHandler :: Maybe Message -> Net ()
logHandler msg = case msg of
  Nothing -> return ()
  Just m -> do
    sql <- asks db
    case sql of
      Nothing -> return ()
      Just db -> do
        c <- liftIO $ run db "insert into rawlog values (datetime('now'), ?)" [toSql $ encode m]
        liftIO $ commit db

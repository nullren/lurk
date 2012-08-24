module Lurk.Logger where

import Lurk.Types
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3

writeLog :: String -> Net ()
writeLog s = do
  sql <- asks db
  case sql of
    Nothing -> return ()
    Just db -> do
      c <- liftIO $ run db "insert into rawlog values (datetime('now'), ?)" [toSql s]
      liftIO $ commit db

connectLog :: BotConfig -> IO (Maybe Connection)
connectLog cfg = if (logging cfg)
    then do
      db <- connectSqlite3 (database cfg)
      return $ Just db
    else
      return Nothing

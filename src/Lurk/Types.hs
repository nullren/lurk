module Lurk.Types
  ( ConnInfo(..)
  , BotConfig(..)
  , Bot(..)
  , Net
  , Handler(..)
  , msgHandler

  -- Network.IRC
  , Message(..)
  , Prefix(..)
  , Parameter
  , ServerName
  , UserName
  , RealName
  , Command
  ) where

import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.Sqlite3
import Network.IRC hiding (nick,privmsg)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.IO
import System.Time

data ConnInfo = ConnInfo
  { connRead :: IO B.ByteString
  , connWrite :: L.ByteString -> IO ()
  , connClose :: IO ()
  }

data BotConfig = BotConfig 
  { nick :: String
  , username :: String
  , realname :: String
  , server :: String
  , port :: Int
  , ssl :: Bool
  , nickserv :: Bool
  , nickservpassword :: String
  , channels :: [String]
  , handlers :: [Maybe Message -> Net ()]
  , logging :: Bool
  , database :: String
  }

data Bot = Bot 
  { connInfo :: ConnInfo
  , startTime :: ClockTime
  , config :: BotConfig
  , db :: Maybe Connection
  }

type Net = ReaderT Bot IO

data Handler = Handler { kind :: String
                       , condition :: (String, String) -> Bool
                       , response :: (String, String) -> IO (Maybe String)
                       }

msgHandler = Handler { kind = "PRIVMSG"
                     , condition = const False
                     , response = \_ -> return Nothing
                     }

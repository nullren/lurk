module Lurk.Types
  ( ConnInfo(..)
  , BotConfig(..)
  , Bot(..)
  , Net
  , defaultLurkBot

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

-- | Default settings that should be overwritten.
defaultLurkBot = BotConfig
  { nick = "lurkbot"
  , username = "lurkbot"
  , realname = "avocados butt"
  , server = "chat.freenode.net"
  , ssl = False
  , nickserv = False
  , nickservpassword = "nothing!"
  , port = 6667
  , channels = ["#avocadosbonertrust"]
  , handlers = []
  , logging = False
  , database = "lurk.db"
  }

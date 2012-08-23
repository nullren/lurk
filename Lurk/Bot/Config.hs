module Lurk.Bot.Config where

import Control.Monad.Reader
import System.IO
import System.Time
import Network.IRC hiding (nick,privmsg)
import Database.HDBC
import Database.HDBC.Sqlite3

data BotConfig = BotConfig { nick :: String
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
  
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle
               , starttime :: ClockTime
               , config :: BotConfig
               , db :: Maybe Connection
               }

defaultLurkBot = BotConfig
  { nick = "lurkbot"
  , username = "lurkbot"
  , realname = "avocados butt"
  , server = "chat.freenode.net"
  , ssl = False
  , nickserv = False
  , nickservpassword = "nothing!"
  , port = 6667
  , channels = []
  , handlers = []
  , logging = False
  , database = "lurk.db"
  }

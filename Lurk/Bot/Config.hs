module Lurk.Bot.Config where

import Control.Monad.Reader
import System.IO
import System.Time
import Network.IRC hiding (nick,privmsg)

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
                           , database :: String
                           }
  
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle
               , starttime :: ClockTime
               , config :: BotConfig
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
  , database = "lurk.db"
  }

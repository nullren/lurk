module Lurk.Bot.Config where

import Control.Monad.Reader
import System.IO
import System.Time
import Network.IRC

data BotConfig = BotConfig { nick :: String
                           , username :: String
                           , realname :: String
                           , server :: String
                           , port :: Int
                           , channels :: [String]
                           , handlers :: [Maybe Message -> Net ()]
                           }
  
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime, config :: BotConfig }
 

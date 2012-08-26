module Lurk.Bot.Defaults where

import Lurk.Types
import Lurk.Handlers

defaultHandlers :: [Maybe Message -> Net ()]
defaultHandlers = allHandlers

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
  , handlers = defaultHandlers
  , logging = False
  , database = "lurk.db"
  }

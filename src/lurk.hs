module Main (main) where

import Lurk.Bot
import Lurk.Bot.Defaults

main = runBot lurkBot

-- | configure your bot here!
lurkBot = defaultLurkBot
  { nick                = "lurkbot"
  , server              = "irc.fantasy.net"
  , port                = 6697
  , ssl                 = True
  , channels            = [ "#cooldudes"
                          , "#spam"
                          ]
  , nickserv            = False
  , nickservpassword    = "password"
  , database            = "/tmp/lurk.db"
  , logging             = False
  }

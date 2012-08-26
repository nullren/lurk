module Main (main) where

import Lurk.Bot

main = runBot lurkBot

-- | configure your bot here!
lurkBot = defaultLurkBot
            { nick = "lurkbot"
            , server = "irc.starfyre.net"
            , port = 6697
            , ssl = True
            , channels = [ "#mathematics"
                         --, "#reddit-ucla"
                         ]
            , nickserv = False
            , nickservpassword = "dingdong"
            , handlers = [ logHandler
                         , googleHandler
                         , slapHandler
                         , chooseHandler
                         , miscHandler
                         , urlHandler
                         ]
            , database = "/tmp/lurk.db"
            , logging = False
            }

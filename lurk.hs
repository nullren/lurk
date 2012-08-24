import Lurk.Bot

import Lurk.Handler.Google (googleHandler)
import Lurk.Handler.Slap (slapHandler)
import Lurk.Handler.Urls (urlHandler)
import Lurk.Handler.Logging (logHandler)
import Lurk.Handler.EverythingElse (miscHandler)
import Lurk.Handler.Choose (chooseHandler)

main = runBot lurkBot

-- | configure your bot here!
lurkBot = defaultLurkBot
            { nick = "lurkbot"
            , server = "chat.freenode.net"
            , port = 6667
            , channels = [ "#avocadobonertrust"
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

import Lurk.Bot
import Lurk.Handler.Choose (chooseHandler)
import Lurk.Handler.EverythingElse (miscHandler)
import Lurk.Handler.Google (googleHandler)
import Lurk.Handler.Logging (logHandler)
import Lurk.Handler.Slap (slapHandler)
import Lurk.Handler.Urls (urlHandler)

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

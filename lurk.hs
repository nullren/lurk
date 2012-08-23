import Lurk.Bot
import Lurk.Bot.Config
import Lurk.Handler.Google (googleHandler)
import Lurk.Handler.Slap (slapHandler)
import Lurk.Handler.Urls (urlHandler)
import Lurk.Handler.EverythingElse (miscHandler)
main = runBot lurkBot

-- | configure your bot here!

lurkBot = BotConfig "lurkbot" --nick
                    "lurkbot" --user name
                    "avocados butt" --real name
                    "chat.freenode.net"  --server
                    6667 --port

                    -- channels to join
                    [ "#avocadobonertrust"
                    --, "#reddit-ucla"
                    ]

                    -- handlers to use
                    [ googleHandler
                    , slapHandler
                    , urlHandler
                    , miscHandler
                    ]

import Lurk.Bot
import Lurk.Bot.Config
 
lurkBot = BotConfig "lurkbot" --nick
                    "lurkbot" --user name
                    "avocados butt" --real name
                    "chat.freenode.net"  --server
                    6667 --port

                    -- channels to join
                    [ "#avocadobonertrust"
                    , "#reddit-ucla"
                    ]

{-
                    -- handlers to use
                    [ googleHandler
                    , slapHandler
                    , quitHandler
                    , idHandler
                    ]
-}

main = runBot lurkBot

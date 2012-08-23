import Lurk.Bot
import Lurk.Bot.Config
 
lurkBot :: BotConfig
lurkBot = BotConfig "lurkbot" --nick
                    "lurkbot" --user name
                    "avocados butt" --real name
                    "chat.freenode.org"  --server
                    6667 --port
                    [ "#avocadobonertrust"
                    --, "#reddit-ucla"
                    ]

main = runBot lurkBot

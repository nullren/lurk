import Lurk.Bot
import Lurk.Bot.Config
 
lurkBot :: BotConfig
lurkBot = BotConfig "lurkbot" "lurkbot" "avocados butt"
                      "chat.freenode.org" 6667 
                      [ "#avocadobonertrust"
                      --, "#reddit-ucla"
                      ]

main = runBot lurkBot

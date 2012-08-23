module Lurk.Config 
( IRCConfig(..),
  lurkBot
) where

data BotConfig = BotConfig { nick :: String
                           , username :: String
                           , realname :: String
                           , server :: String
                           , port :: Int
                           , channels :: [String]
                           } deriving (Show)
  
lurkBot :: BotConfig
lurkBot = BotConfig "lurkbot" "lurkbot" "avocados butt"
                      "chat.freenode.org" 6667 
                      [ "#reddit-ucla"
                      , "#avocadobonertrust"
                      ]

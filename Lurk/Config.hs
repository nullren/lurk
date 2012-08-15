module Lurk.Config 
( IRCConfig(..),
  lurkBot
) where

data IRCConfig = IRCConfig { nick :: String
                           , username :: String
                           , realname :: String
                           , server :: String
                           , port :: Int
                           , channels :: [String]
                           } deriving (Show)
  
lurkBot :: IRCConfig
lurkBot = IRCConfig "lurkbot" "lurkbot" "avocados butt"
                      "chat.freenode.org" 6667 
                      [ "#reddit-ucla"
                      , "#avocadobonertrust"
                      ]

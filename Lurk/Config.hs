module Lurk.Config 
( IRCConfig(..),
  lurkBot
) where

data IRCConfig = IRCConfig { nick :: String
                           , server :: String
                           , port :: Int
                           , channels :: [String]
                           } deriving (Show)
  
lurkBot :: IRCConfig
lurkBot = IRCConfig "lurkbot" "chat.freenode.org" 6667 ["#reddit-ucla-avocado", "#avocadobonertrust", "#avocadospam"]

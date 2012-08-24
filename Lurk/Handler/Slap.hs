module Lurk.Handler.Slap (slapHandler) where

import Lurk.Bot.IRC
import Lurk.Types
import Control.Monad.Reader
import Network.IRC hiding (nick, privmsg)
import Data.List

eval :: String -> String -> Net ()
eval c x | "!slap " `isPrefixOf` x = privmsg c ("\001ACTION slaps " ++ (drop 6 x) ++ " around with a large trout!\001")
eval _    _                        = return () -- ignore everything else

slapHandler msg = do
  cfg <- asks config
  case msg of
    Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
      -> eval tgt $ concat mess
         where tgt = if (nick cfg) `isPrefixOf` chan then n else chan
    _ -> return ()

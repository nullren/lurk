module Lurk.Handler where

import Lurk.Bot.Config
import Lurk.Eval
import Lurk.Bot.IRC
import Network.IRC hiding (nick)
import Control.Monad.Reader
import Data.List

handle :: String -> Net ()
handle s = do
  cfg <- asks config
  case decode (s++"\r\n") of
    -- end of MOTD
    Just (Message _ "376" _)
      -> mapM_ (\x -> write "JOIN" x) (channels cfg)

    -- nick taken
    Just (Message _ "433" _)
      -> write "NICK" (nick cfg ++ "_")

    Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
      -> eval tgt $ concat mess
         where tgt = if (nick cfg) `isPrefixOf` chan then n else chan

    -- do nothing
    _ -> return ()

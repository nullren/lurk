module Lurk.Handler.EverythingElse (miscHandler) where

import Control.Monad.Reader
import Data.List
import Lurk.Bot.IRC
import Lurk.Types
import System.Exit

eval :: String -> String -> Net ()
eval _    "!quit"                  = write "QUIT" ":Exiting" >> liftIO exitSuccess
eval c x | "!id " `isPrefixOf` x   = privmsg c (drop 4 x)
eval _    _                        = return () -- ignore everything else

-- | Need to put these in better places, but for now they are sitting
-- here. This is where we tell the bot to Join channels and change its
-- nick if the nick is taken.
miscHandler :: Maybe Message -> Net ()
miscHandler msg = do
  cfg <- asks config
  case msg of
    Just (Message _ "376" _) 
      -> if nickserv cfg
         then privmsg "NickServ" $ "identify " ++ nickservpassword cfg
         else mapM_ (write "JOIN") (channels cfg)
    Just (Message _ "396" _) -> mapM_ (write "JOIN") (channels cfg)
    Just (Message _ "433" _) -> write "NICK" (nick cfg ++ "_")
    Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
      -> eval tgt $ concat mess
         where tgt = if nick cfg `isPrefixOf` chan then n else chan
    _ -> return ()

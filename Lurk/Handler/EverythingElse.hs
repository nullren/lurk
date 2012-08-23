module Lurk.Handler.EverythingElse (miscHandler) where

import Control.Monad.Reader
import Data.List
import Lurk.Bot.Config
import Lurk.Bot.IRC
import Network.IRC hiding (nick,privmsg)
import System.Exit

eval :: String -> String -> Net ()
eval _    "!quit"                  = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval c x | "!id " `isPrefixOf` x   = privmsg c (drop 4 x)
eval _    _                        = return () -- ignore everything else

miscHandler :: Maybe Message -> Net ()
miscHandler msg = do
  cfg <- asks config
  case msg of
    Just (Message _ "376" _) -> privmsg "NickServ" "identify nickservpassword"
    Just (Message _ "396" _) -> mapM_ (\x -> write "JOIN" x) (channels cfg)
    Just (Message _ "433" _) -> write "NICK" (nick cfg ++ "_")
    Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
      -> eval tgt $ concat mess
         where tgt = if (nick cfg) `isPrefixOf` chan then n else chan
    _ -> return ()

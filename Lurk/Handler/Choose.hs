module Lurk.Handler.Choose (chooseHandler) where

import Lurk.Bot.IRC
import Lurk.Bot.Config
import Control.Monad.Reader
import Network.IRC hiding (nick, privmsg)
import Data.List
import Data.List.Split
import System.Random

pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

eval :: String -> String -> Net ()
eval c x | "!c " `isPrefixOf` x = do 
  (liftIO $ pick $ splitOn " or " (drop 3 x)) >>= privmsg c
eval _    _                     = return () -- ignore everything else

chooseHandler :: Maybe Message -> Net ()
chooseHandler msg = do
  cfg <- asks config
  case msg of
    Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
      -> eval tgt $ concat mess
         where tgt = if (nick cfg) `isPrefixOf` chan then n else chan
    _ -> return ()

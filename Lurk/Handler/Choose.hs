module Lurk.Handler.Choose (chooseHandler) where

import Lurk.Bot.IRC
import Lurk.Types
import Lurk.Utils
import Control.Monad.Reader
import Data.List
import Data.List.Split

chooseHandler :: Maybe Message -> Net ()
chooseHandler msg = do
  cfg <- asks config
  case msg of

    Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
      | "!c " `isPrefixOf` concat mess
      -> (liftIO . pick . splitOn " or ") m >>= privmsg tgt
         where tgt = if nick cfg `isPrefixOf` chan then n else chan
               m = drop 3 $ concat mess

    _ -> return ()

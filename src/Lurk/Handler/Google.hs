module Lurk.Handler.Google (googleHandler) where

import Control.Monad
import Control.Monad.Reader
import Data.List
import Lurk.Bot.IRC
import Lurk.Google
import Lurk.Types
import Lurk.Url
import Network.TinyURL

googleHandler msg = do
  cfg <- asks config
  case msg of
    Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
      -> eval tgt $ concat mess
         where tgt = if nick cfg `isPrefixOf` chan then n else chan
    _ -> return ()

eval c x | "!gs " `isPrefixOf` x   = privmsg c $ getGoogleSearchUrl (drop 4 x)
eval c x | "!gsbi " `isPrefixOf` x = privmsg c $ getGoogleSearchByImageUrl (drop 6 x)
eval c x | "!g " `isPrefixOf` x    = do
  r <- liftIO $ getSearchResults (drop 3 x)
  mapM_ (\(t,u) -> case u of
    Nothing -> privmsg c t
    Just url -> do
      url' <- liftIO $ tinyURL url
      privmsg c (t ++ " <" ++ url' ++ ">")) r
eval _ _ = return ()

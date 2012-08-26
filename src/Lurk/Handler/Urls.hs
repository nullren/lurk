module Lurk.Handler.Urls (urlHandler) where

import Control.Monad.Reader
import Data.List
import Lurk.Bot.IRC
import Lurk.Types
import Lurk.Url
import Lurk.Utils

eval :: String -> String -> Net ()
eval c x | urls@(_:_) <- getUrls x = mapM_ (\x -> do
  title <- liftIO $ getTitle x 
  privmsg c title 
  ) urls
eval _    _                        = return () -- ignore everything else


-- | Greedy. Every message, even commands, are searched for URLs.
urlHandler :: Maybe Message -> Net ()
urlHandler msg = do
  cfg <- asks config
  case msg of
    Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
      -> eval tgt $ concat mess
         where tgt = if nick cfg `isPrefixOf` chan then n else chan
    _ -> return ()

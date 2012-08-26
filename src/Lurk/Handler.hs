module Lurk.Handler where

import Control.Monad.Reader
import Data.List
import Lurk.Bot.IRC
import Lurk.Types

-- | Processes the string in each of the handlers listed in BotConfig.
-- Ideally, I would like them to run in order and stop at the first one
-- that performs an IO.
handle :: String -> Net ()
handle s = do
  cfg <- asks config
  sequence_ $ fmap ($ decode (s++"\r\n")) (handlers cfg)

-- | Function to simplify the declaration of message handlers
msgHandler_ :: Handler -> Maybe Message -> Net ()
msgHandler_ h m = do
  case h of

    Handler "PRIVMSG" _ _ -> do
      cfg <- asks config
      case m of
        Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
          | condition h $ concat mess
          -> liftIO (response h (n, concat mess)) >>= privmsg tgt
             where tgt = if nick cfg `isPrefixOf` chan then n else chan
        _ -> return ()

    _ -> return ()


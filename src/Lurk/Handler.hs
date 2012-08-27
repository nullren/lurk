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

-- | Function to simplify the declaration of message handlers. The goal
-- should be to remove this eventually and have handle do everything.
handler :: Handler -> Maybe Message -> Net ()
handler h m = do
  case h of

    Handler "PRIVMSG" _ _ -> do
      cfg <- asks config
      case m of
        Just (Message (Just (NickName n _ _)) "PRIVMSG" (chan:mess))
          | condition h $ (n, concat mess)
          -> do r <- liftIO (response h (n, concat mess))
                case r of
                  Nothing -> return ()
                  Just s -> privmsg tgt s
             where tgt = if nick cfg `isPrefixOf` chan then n else chan
        _ -> return ()

    _ -> return ()


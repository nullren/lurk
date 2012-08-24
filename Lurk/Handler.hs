module Lurk.Handler where

import Control.Monad.Reader
import Lurk.Bot.IRC
import Lurk.Types

-- | Processes the string in each of the handlers listed in BotConfig.
-- Ideally, I would like them to run in order and stop at the first one
-- that performs an IO.
handle :: String -> Net ()
handle s = do
  cfg <- asks config
  sequence_ $ fmap ($ decode (s++"\r\n")) (handlers cfg)

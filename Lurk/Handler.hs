module Lurk.Handler where

import Lurk.Types
import Lurk.Bot.IRC
import Control.Monad.Reader

handle :: String -> Net ()
handle s = do
  cfg <- asks config
  sequence_ $ fmap ($ decode (s++"\r\n")) (handlers cfg)

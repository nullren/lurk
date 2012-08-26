module Lurk.Handler.Choose (chooseHandler) where

import Data.List
import Data.List.Split
import Lurk.Handler
import Lurk.Types
import Lurk.Utils

choose = msgHandler
  { condition = isPrefixOf "!c "
  , response = \(nick, message) -> pick . (map Just) .  splitOn " or " $ drop 3 message
  }

chooseHandler :: Maybe Message -> Net ()
chooseHandler = handler choose

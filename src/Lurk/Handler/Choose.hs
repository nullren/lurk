module Lurk.Handler.Choose (chooseHandler) where

import Data.List
import Data.List.Split
import Lurk.Handler
import Lurk.Types
import Lurk.Utils

choose = Handler
  { kind = "PRIVMSG"
  , condition = isPrefixOf "!c "
  , response = \(nick, message) -> pick . splitOn " or " $ drop 3 message
  }

chooseHandler :: Maybe Message -> Net ()
chooseHandler = msgHandler_ choose

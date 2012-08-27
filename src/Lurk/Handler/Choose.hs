module Lurk.Handler.Choose (chooseHandler) where

import Data.List
import Data.List.Split
import Lurk.Handler
import Lurk.Types
import Lurk.Utils

isChoose :: (String, String) -> Bool
isChoose (nick, message) = "!c " `isPrefixOf` message

randomChoice :: (String, String) -> IO (Maybe String)
randomChoice (nick, message) = pick . (map Just) . splitOn " or " $ drop 3 message

choose = msgHandler
  { condition = isChoose
  , response  = randomChoice
  }

chooseHandler = handler choose

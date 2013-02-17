module Lurk.Handler.Highfive (highfiveHandler) where

import Data.List
import Lurk.Types
import Lurk.Handler

isHighfive :: (String, String) -> Bool
isHighfive (_,m) = "o/" `isPrefixOf` m

giveHighfive :: (String, String) -> IO (Maybe String)
giveHighfive (_,_) = return $ Just "\\o"

highfive = msgHandler
  { condition = isHighfive
  , response = giveHighfive
  }

highfiveHandler = handler highfive

module Lurk.Handler.Slap (slapHandler) where

import Data.List
import Lurk.Types
import Lurk.Handler


slap = msgHandler
  { condition = isPrefixOf "!slap "
  , response = \(n, m) -> return . justSlap . victims $ m
  }

justSlap [] = Nothing
justSlap ms = Just . build $ ms
victims = tail . words
act s = "\001ACTION slaps " ++ s ++ " around with a large trout!\001"
build = act . unwords

slapHandler = handler slap

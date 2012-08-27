module Lurk.Handler.Slap (slapHandler) where

import Data.List
import Lurk.Types
import Lurk.Handler


victims = tail . words

act s = "\001ACTION slaps " ++ s ++ " around with a large trout!\001"

build = act . unwords

justSlap [] = Nothing
justSlap ms = Just (build ms)

slap = msgHandler
  { condition = isPrefixOf "!slap "
  , response = \(n, m) -> return $ justSlap (victims m)
  }

slapHandler = handler slap

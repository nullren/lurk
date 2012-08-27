module Lurk.Handler.Slap (slapHandler) where

import Data.List
import Lurk.Types
import Lurk.Handler

isSlapCommand c = "!slap " `isPrefixOf` c

victims = tail . words

act s = "\001ACTION slaps " ++ s ++ " around with a large trout!\001"

build = act . unwords

justSlap [] = Nothing
justSlap ms = Just (build ms)

slapPeople (nick, message) = justSlap (victims message)

slap = msgHandler
  { condition = isSlapCommand
  , response = return . slapPeople
  }

slapHandler = handler slap

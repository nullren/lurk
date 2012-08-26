module Lurk.Handlers 
  ( allHandlers
  , module Lurk.Handler.Choose
  , module Lurk.Handler.EverythingElse
  , module Lurk.Handler.Google
  , module Lurk.Handler.Logging
  , module Lurk.Handler.Slap
  , module Lurk.Handler.Urls
  ) where

import Lurk.Handler.Choose (chooseHandler)
import Lurk.Handler.EverythingElse (miscHandler)
import Lurk.Handler.Google (googleHandler)
import Lurk.Handler.Logging (logHandler)
import Lurk.Handler.Slap (slapHandler)
import Lurk.Handler.Urls (urlHandler)

allHandlers = [ logHandler
              , googleHandler
              , slapHandler
              , chooseHandler
              , miscHandler
              , urlHandler
              ]

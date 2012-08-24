module Lurk.Bot.IRC where

import Control.Monad.Reader
import Control.Monad
import Text.Printf
import Lurk.Types
import qualified Data.ByteString.Lazy.Char8 as L

write :: String -> String -> Net ()
write s t = do
  c <- asks connInfo
  liftIO $ (connWrite c) $ L.pack $ printf "%s %s\r\n" s t
  liftIO $ printf    "> %s %s\n" s t

privmsg :: String -> String -> Net ()
privmsg c s = write "PRIVMSG" (c ++ " :" ++ s)


module Lurk.Handler where

import Lurk.Bot.Config
import Lurk.Eval
import Lurk.Bot.IRC
import Network.IRC hiding (nick)
import Control.Monad.Reader
import Data.List

handle :: String -> Net ()
handle s = do
  cfg <- asks config
  case decode (s++"\r\n") of
    Nothing -> return ()
    Just msg -> case msg_command msg of

      -- end of MOTD
      "376"         -> mapM_ (\x -> write "JOIN" x) (channels cfg)

      -- nick taken
      "433"         -> write "NICK" (nick cfg ++ "_")

      "PRIVMSG"     -> eval tgt mess where
                         chan = head $ msg_params msg
                         mess = last $ msg_params msg
                         tgt = if (nick cfg) `isPrefixOf` chan 
                                 then case msg_prefix msg of
                                   Nothing -> chan
                                   Just n -> getnick n
                                 else chan
                         getnick (NickName s _ _) = s

      -- do nothing
      _             -> return ()

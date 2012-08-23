module Lurk.Handle where

import Lurk.Bot.Config

handle :: String -> Net ()
handle s = do
  return ()
{-
  case decode (s++"\r\n") of
    Nothing -> return ()
    Just msg -> case msg_command msg of

      -- end of MOTD
      "376"         -> mapM_ (\x -> write "JOIN" x) (channels lurkBot)

      -- nick taken
      "433"         -> write "NICK" (nick lurkBot ++ "_")

      "PRIVMSG"     -> eval tgt mess where
                         chan = head $ msg_params msg
                         mess = last $ msg_params msg
                         tgt = if (nick lurkBot) `isPrefixOf` chan 
                                 then case msg_prefix msg of
                                   Nothing -> chan
                                   Just n -> getnick n
                                 else chan
                         getnick (NickName s _ _) = s

      -- do nothing
      _             -> return ()
-}

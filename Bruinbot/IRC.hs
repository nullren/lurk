module Bruinbot.IRC (
  readExpr,
  Message(..),
  Prefix(..)
) where

import Text.ParserCombinators.Parsec hiding (spaces)

data Message = Message
  { msgPrefix :: String
  , msgCommand :: String
  , msgParameters :: String
  , msgComplete :: String
  }

data Prefix = Server { srvName :: String }
            | User { nickName :: String, userName :: (Maybe String), hostName :: (Maybe String) }
  
readExpr :: String -> Message
readExpr input = case parse parseExpr "lisp" input of
    Left err -> Message ("No match: " ++ show err) "" "" ""
    Right val -> val

parsePrefix :: Parser Message
parsePrefix = do char ':'
                 x <- many (noneOf " ")
                 char ' '
                 y <- many (noneOf " ")
                 char ' '
                 z <- many (noneOf "\r\n")
                 return $ Message x y z (":" ++ x ++ " " ++ y ++ " " ++ z)

parseExpr :: Parser Message
parseExpr = parsePrefix

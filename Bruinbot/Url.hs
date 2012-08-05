module Bruinbot.Url (getTitle) where

import Data.List
import Data.Maybe

import Network.URI
import Network.HTTP
import Network.HTTP.Headers
import Network.Browser
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

import Codec.Binary.UTF8.String

import qualified Control.Exception as E

main = do
  title <- getTitle "http://google.com"
  putStrLn title

getTitle :: String -> IO String
getTitle url = do
  result <- E.try (getUrlTitle url) :: IO (Either E.SomeException String)
  case result of
    Left ex -> return $ "Caught Exception: " ++ show ex
    Right v -> return v

maxTitleLength :: Int
maxTitleLength = 80

maxRedirectFollow :: Int
maxRedirectFollow = 5

redirectDepth = maxRedirectFollow

getUrlTitle :: String -> IO String
getUrlTitle url = case parseURI url of
  Nothing -> fail "Given a bad url."
  Just uri -> do
    (_, rsp) <- browse $ do
      setAllowRedirects True
      request $ mkRequest GET uri
    case (getHdrDeets rsp) of
      Nothing -> fail "What is this shit?"
      Just details -> case (extractTitle $ rspBody rsp) of
        Nothing -> return ("File details: " ++ details)
        Just title -> return (title ++ " " ++ details)

-- print something like: [image/text] and file size
getHdrDeets :: HasHeaders a => a -> Maybe String
getHdrDeets r = do
  case (findHeader HdrContentType r) of
    Nothing -> fail "Given URL has no content type."
    Just ct -> case (findHeader HdrContentLength r) of
      Nothing -> fail "Given URL has no content."
      Just cl -> return ("[" ++ ct ++ "] " ++ cl)

extractTitle :: String -> Maybe String
extractTitle = content . tags . decodeString where
  tags = closing . opening . canonicalizeTags . parseTags
  opening = dropWhile (not . tagOpenLit "title" (const True))
  closing = takeWhile (not . tagCloseLit "title")
  content = maybeText . format . innerText
  format = unwords . words
  maybeText [] = Nothing
  maybeText t = Just ("Title: " ++ take maxTitleLength (encodeString t))

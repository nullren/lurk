{- provide URL utilities -}

module Lurk.Url (
  getUrls,
  getTitle,
  getContent,
  getContentType,
  extractTitle
) where

import Data.List
import Control.Monad.Reader
import Network.Curl
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Codec.Binary.UTF8.String


--url = "https://upload.wikimedia.org/wikipedia/commons/4/42/PIA15279_3rovers-stand_D2011_1215_D521.jpg"
url = "https://google.com"

main = do
  getTitle url >>= putStrLn

maxTitleLength :: Int
maxTitleLength = 80

maxRedirectFollow :: Long
maxRedirectFollow = 5

curl_options :: [CurlOption]
curl_options = [CurlFollowLocation True, CurlMaxRedirs maxRedirectFollow, CurlHeader False]

getTitle :: String -> IO String
getTitle uri = do
  t <- getContentType uri
  if isHTML t then do
    c <- getContent uri
    case extractTitle c of
      Nothing -> return ("File type: " ++ t )
      Just title -> return title
  else do
    return ("File type: " ++ t )
 where
  isHTML t = "[text/html" `isPrefixOf` t

extractTitle :: String -> Maybe String
extractTitle = content . tags where
  tags = closing . opening . canonicalizeTags . parseTags
  opening = dropWhile (not . tagOpenLit "title" (const True))
  closing = takeWhile (not . tagCloseLit "title")
  content = maybeText . format . innerText
  format = unwords . words
  maybeText [] = Nothing
  maybeText t = Just ("Title: " ++ take maxTitleLength (encodeString t))

getContentType :: String -> IO String
getContentType uri = do
  a <- curlHead uri curl_options
  t <- getContentTypeHdr a
  l <- getContentLenHdr a
  return $ ("[" ++ strip t ++ "] " ++ strip l )
 where
  getContentTypeHdr :: (String, [(String, String)]) -> IO String
  getContentTypeHdr (_, h) = case lookup "Content-Type" h of
    Just x -> return x
    Nothing -> return "Oh god"
  getContentLenHdr :: (String, [(String, String)]) -> IO String
  getContentLenHdr (_, h) = case lookup "Content-Length" h of
    Just x -> return x
    Nothing -> return "Oh god"
  strip = unwords . words

getContent :: String -> IO String
getContent uri = do
  (_,c) <- curlGetString uri curl_options
  return c

getUrls :: String -> [String]
getUrls s = 
  let http = filter (\x -> "http://" `isPrefixOf` x) $ words s
      https = filter (\x -> "https://" `isPrefixOf` x) $ words s
  in http ++ https

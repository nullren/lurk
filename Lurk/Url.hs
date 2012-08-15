{- provide URL utilities -}

module Lurk.Url (
  getTitle,
  getContent,
  getShortContent,
  getContentType,
  extractTitle
) where

import Control.Monad.Reader
import Network.Curl
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Codec.Binary.UTF8.String
import Lurk.Utils

main = do
  getTitle "http://google.com" >>= putStrLn

maxTitleLength :: Int
maxTitleLength = 80

maxRedirectFollow :: Long
maxRedirectFollow = 5

curl_options :: [CurlOption]
curl_options = [CurlFollowLocation True
               , CurlMaxRedirs maxRedirectFollow
               , CurlHeader False
               ]

getTitle :: String -> IO String
getTitle uri = do
  c <- getShortContent uri
  case extractTitle c of
    Just title -> return title
    Nothing -> getContentType uri >>= return

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
  return $ ("File type: [" ++ strip t ++ "] " ++ l )
 where
  getContentTypeHdr :: (String, [(String, String)]) -> IO String
  getContentTypeHdr (_, h) = case lookup "Content-Type" h of
    Just x -> return x
    Nothing -> return "Oh god"
  getContentLenHdr :: (String, [(String, String)]) -> IO String
  getContentLenHdr (_, h) = case lookup "Content-Length" h of
    Just x -> return $ prSi $ strip x
    Nothing -> return "I don't know what you want!"
  strip = unwords . words
  prSi = prettySize . (\x -> read x :: Integer) . strip

getContent :: String -> IO String
getContent uri = do
  (_,c) <- curlGetString uri curl_options
  return c

getShortContent :: String -> IO String
getShortContent uri = do
  (_,c) <- curlGetString uri (curl_options ++ [CurlRange "0-2048"])
  return c


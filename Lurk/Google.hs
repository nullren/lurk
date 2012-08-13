module Lurk.Google (
  getSearchResults,
  extractTopText
) where

import Lurk.Url
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Codec.Binary.UTF8.String
import Network.HTTP.Base

getSearchResults :: String -> IO String
getSearchResults query = do
  r <- getContent $ "http://google.com/search?q=" ++ (urlEncode query)
  return r

extractTopText :: String -> Maybe String
extractTopText = content . tags . decodeString where
  tags = closing . opening . canonicalizeTags . head . sections (~== "<div id=topstuff>") . parseTags
  opening = dropWhile (not . tagOpenLit "h2" (const True))
  closing = takeWhile (not . tagCloseLit "h2")
  content = maybeText . format . innerText
  format = unwords . words
  maybeText [] = Nothing
  maybeText "Ad" = Nothing
  maybeText t = Just ("Result: " ++ (encodeString t))



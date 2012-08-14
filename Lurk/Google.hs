module Lurk.Google (
  getSearchResults,
  getSearchResults_,
  getRawSearchResults,
  extractTopText
) where

import Lurk.Url
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Codec.Binary.UTF8.String
import Network.HTTP.Base

getRawSearchResults :: String -> IO String
getRawSearchResults q = getContent $ "http://google.com/search?q=" ++ (urlEncode q)

getSearchResults :: String -> IO String
getSearchResults query = do
  r <- getRawSearchResults query
  case extractTopText r of
    Nothing -> return "I wasn't made for this shit!"
    Just s -> return s

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


getSearchResults_ :: String -> IO [String]
getSearchResults_ query = do
  r <- getRawSearchResults query
  case extractTopText r of
    Nothing -> return ["I wasn't made for this shit!", "Fucker."]
    Just s -> return [s]
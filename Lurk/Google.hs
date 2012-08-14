module Lurk.Google (
  getGoogleSearchUrl,
  getSearchResults,
  getRawSearchResults,
  extractTopText,
  extractSearchResults
) where

import Lurk.Url
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Codec.Binary.UTF8.String
import Network.HTTP.Base

getGoogleSearchUrl :: String -> String
getGoogleSearchUrl = (++) "http://google.com/search?q=" . urlEncode

getRawSearchResults :: String -> IO String
getRawSearchResults = getContent . getGoogleSearchUrl

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

extractSearchResults :: String -> Maybe [String]
extractSearchResults [] = Nothing
extractSearchResults p = Just $ map (innerText) (sections (~== "<li class=g>") $ parseTags p)

getSearchResults :: String -> IO [String]
getSearchResults query = do
  r <- getRawSearchResults query
  case extractTopText r of
    Nothing -> return ["I wasn't made for this shit!", "Fucker."]
    Just s -> return [s]

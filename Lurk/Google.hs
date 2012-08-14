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
import Control.Applicative

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
extractSearchResults p = map content <$> (maybetake $ tags $ decodeString p) where
  
  tags = listitems . closing . opening . search
  search = head . sections (~== "<div id=search>") . canonicalizeTags . parseTags
  opening = dropWhile (not . tagOpenLit "ol" (const True))
  closing = takeWhile (not . tagCloseLit "ol")
  listitems x = map (takeWhile (not . tagCloseLit "li")) (sections (~== "<li class=g>") x)

  content s = do
    let title = (innerText . closetitle . opentitle) s
    title
  opentitle = dropWhile (not . tagOpenLit "h3" (const True))
  closetitle = takeWhile (not . tagCloseLit "h3")

  maybetake [] = Nothing
  maybetake s = Just (take 3 s)

getSearchResults :: String -> IO [String]
getSearchResults query = do
  r <- getRawSearchResults query
  case extractTopText r of
    Nothing -> case extractSearchResults r of
      Just s -> return s
      Nothing -> return ["I don't know what I'm doing!"]
    Just s -> return [s]

module Lurk.Google (
  getGoogleSearchUrl,
  getGoogleSearchByImageUrl,
  getSearchResults,
  getSbiResults,
  extractTopText,
  extractSbiKeywords,
  extractSearchResults
) where

import Lurk.Url
import Lurk.Utils
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Codec.Binary.UTF8.String
import Network.HTTP.Base
import Control.Applicative

genericSearchUrl :: String -> String -> String
genericSearchUrl uri = (++) uri . urlEncode

getGoogleSearchUrl :: String -> String
getGoogleSearchUrl = genericSearchUrl "http://google.com/search?q="

getGoogleSearchByImageUrl :: String -> String
getGoogleSearchByImageUrl = genericSearchUrl "http://google.com/searchbyimage?image_url="

maxSearchResults = 3 :: Int

getGenericResults :: (String -> Maybe String)
                  -> String
                  -> IO [(String, Maybe String)]
getGenericResults extractor uri = do
  r <- getContent $ uri
  case extractor r of
    Nothing -> case extractSearchResults r of
      Just s -> return s
      Nothing -> return [("I don't know what I'm doing!", Nothing)]
    Just s -> return [(s, Nothing)]

getSbiResults :: String -> IO [(String, Maybe String)]
getSbiResults u = getGenericResults extractSbiKeywords (getGoogleSearchByImageUrl u)

getSearchResults :: String -> IO [(String,Maybe String)]
getSearchResults u = getGenericResults extractNothing (getGoogleSearchUrl u)

extractNothing :: String -> Maybe String
extractNothing url = Nothing

extractTopText :: String -> Maybe String
extractTopText = content . tags where
  tags = closing . opening . canonicalizeTags . head' . sections (~== "<div id=topstuff>") . parseTags
  opening = dropWhile (not . tagOpenLit "h2" (const True))
  closing = takeWhile (not . tagCloseLit "h2")
  content = maybeText . format . innerText
  format = unwords . words
  maybeText [] = Nothing
  maybeText "Ad" = Nothing
  maybeText t = Just ("Result: " ++ (encodeString t))

-- get keywords from the google sbi image page
extractSbiKeywords :: String -> Maybe String
extractSbiKeywords = content . tags where
  tags = closing . opening . canonicalizeTags . head' . sections (~== "<div id=topstuff>") . parseTags
  opening = dropWhile (not . tagOpenLit "a" (any (\(n,v) -> n=="style" && v=="font-weight:bold;font-style:italic")))
  closing = takeWhile (not . tagCloseLit "a")
  content = maybeText . format . innerText
  format = unwords . words
  maybeText [] = Nothing
  maybeText t = Just ("Result: " ++ (encodeString t))

-- return a list of page titles and urls
extractSearchResults :: String -> Maybe [(String, Maybe String)]
extractSearchResults [] = Nothing
extractSearchResults p = map content <$> (maybetake $ tags p) where
  -- searches for the list items in search results
  tags = listitems . closing . opening . search
  search = head' . sections (~== "<div id=search>") . canonicalizeTags . parseTags
  opening = dropWhile (not . tagOpenLit "ol" (const True))
  closing = takeWhile (not . tagCloseLit "ol")
  listitems x = map (takeWhile (not . tagCloseLit "li")) (sections (~== "<li class=g>") x)
  -- append the domain to each of the urls and return them with the url
  -- text
  content s = do
    let url = ((++) "http://google.com" $ getanchor $ opentitle s)
    ((innerText . closetitle . opentitle) s , Just url)
  opentitle = dropWhile (not . tagOpenLit "h3" (const True))
  getanchor = fromAttrib "href" . head . dropWhile (not . tagOpenLit "a" (const True))
  closetitle = takeWhile (not . tagCloseLit "h3")
  -- return nothing if an empty list, no search results
  maybetake [] = Nothing
  maybetake s = Just (take maxSearchResults s)

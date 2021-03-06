module Lurk.Google where

import Codec.Binary.UTF8.String
import Control.Applicative
import Lurk.Url
import Lurk.Utils
import Network.Curl
import Network.HTTP.Base
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

maxSearchResults = 3 :: Int

-- | Set the UserAgent to whatever will bring up Google's JavaScript
-- enabled search page.
firefoxUserAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/21.0.1180.77 Safari/537.1" :: String

genericSearchUrl :: String -> String -> String
genericSearchUrl uri = (++) uri . urlEncode

getGoogleSearchUrl :: String -> String
getGoogleSearchUrl = genericSearchUrl "http://google.com/search?q="

getGoogleSearchByImageUrl :: String -> String
getGoogleSearchByImageUrl = genericSearchUrl "http://google.com/searchbyimage?image_url="

getGenericResults :: String
                  -> (String -> Maybe String)
                  -> (String -> Maybe [(String, Maybe String)])
                  -> String
                  -> IO [(String, Maybe String)]
getGenericResults ua extractor altext uri = do
  r <- getContent_ [CurlUserAgent ua] uri
  case extractor r of
    Nothing -> case altext r of
      Just s -> return s
      Nothing -> return [("I don't know what I'm doing!", Nothing)]
    Just s -> return [(s, Nothing)]

-- | Returns result from Google's Search By Image feature.
getSbiResults :: String -> IO [(String, Maybe String)]
getSbiResults u = getGenericResults 
                    firefoxUserAgent
                    extractSbiKeywords 
                    extractSearchNothing
                    (getGoogleSearchByImageUrl u)

-- | Runs a calculation or returns Google search results. If using a
-- UserAgent that returns a JavaScript page, extractTopText will need to
-- be updated. Right now, updating this is on the todo list.
getSearchResults :: String -> IO [(String,Maybe String)]
getSearchResults u = getGenericResults "-"
                       extractTopText
                       extractSearchResults
                       (getGoogleSearchUrl u)

extractNothing :: String -> Maybe String
extractNothing url = Nothing

-- | On Javascript enabled searches, the "topstuff" div is always
-- present even when empty. This needs to be handled correctly in such
-- situations.
extractTopText :: String -> Maybe String
extractTopText = content . tags where
  tags = closing . opening . canonicalizeTags . head' . sections (~== "<div id=topstuff>") . parseTags
  opening = dropWhile (not . tagOpenLit "h2" (const True))
  closing = takeWhile (not . tagCloseLit "h2")
  content = maybeText . format . innerText
  format = unwords . words
  maybeText [] = Nothing
  maybeText "Ad" = Nothing
  maybeText t = Just (encodeString t)

-- | Get keywords from the Google SBI image page.
extractSbiKeywords :: String -> Maybe String
extractSbiKeywords = content . tags where
  tags = closing . opening . canonicalizeTags . head' . sections (~== "<div id=topstuff>") . parseTags
  opening = dropWhile (not . tagOpenLit "a" (any (\(n,v) -> n=="style" && v=="font-weight:bold;font-style:italic")))
  closing = takeWhile (not . tagCloseLit "a")
  content = maybeText . format . innerText
  format = unwords . words
  maybeText [] = Nothing
  maybeText t = Just (encodeString t)

extractSearchNothing :: String -> Maybe [(String, Maybe String)]
extractSearchNothing _ = Just [("",Nothing)]

-- | Return a list of page titles and URLs.
extractSearchResults :: String -> Maybe [(String, Maybe String)]
extractSearchResults [] = Nothing
extractSearchResults p = map content <$> maybetake (tags p) where
  -- searches for the list items in search results
  tags = listitems . closing . opening . search
  search = head' . sections (~== "<div id=search>") . canonicalizeTags . parseTags
  opening = dropWhile (not . tagOpenLit "ol" (const True))
  closing = takeWhile (not . tagCloseLit "ol")
  listitems x = map (takeWhile (not . tagCloseLit "li")) (sections (~== "<li class=g>") x)
  -- append the domain to each of the urls and return them with the url
  -- text
  content s = do
    let url = (++) "http://google.com" $ getanchor $ opentitle s
    ((innerText . closetitle . opentitle) s , Just url)
  opentitle = dropWhile (not . tagOpenLit "h3" (const True))
  getanchor = fromAttrib "href" . head . dropWhile (not . tagOpenLit "a" (const True))
  closetitle = takeWhile (not . tagCloseLit "h3")
  -- return nothing if an empty list, no search results
  maybetake [] = Nothing
  maybetake s = Just (take maxSearchResults s)

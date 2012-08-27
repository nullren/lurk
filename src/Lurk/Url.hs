module Lurk.Url where

import Codec.Binary.UTF8.String
import Control.Monad.Reader
import Data.IORef
import Lurk.Utils
import Network.Curl hiding (curlGetString)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

maxTitleLength :: Int
maxTitleLength = 250

maxRedirectFollow :: Long
maxRedirectFollow = 5

curl_options :: [CurlOption]
curl_options = [CurlFollowLocation True
               , CurlMaxRedirs maxRedirectFollow
               , CurlHeader False
               , CurlSSLVerifyPeer False
               ]

-- | Returns the title of a page. If there is no title element, look up
-- the returned mime-type and size of the file.
getTitle :: String -> IO String
getTitle uri = do
  c <- getContent_ [CurlRange "0-2048"] uri
  case extractTitle c of
    Just title -> return title
    Nothing -> getContentType uri

-- | Looks for the text in the HTML title tag.
extractTitle :: String -> Maybe String
extractTitle = content . tags where
  tags = closing . opening . canonicalizeTags . parseTags
  opening = dropWhile (not . tagOpenLit "title" (const True))
  closing = takeWhile (not . tagCloseLit "title")
  content = maybeText . format . innerText
  format = unwords . words
  maybeText [] = Nothing
  maybeText t = Just ("Title: " ++ take maxTitleLength (encodeString t))

-- | Returns mime-type and file size of a particular URL.
getContentType :: String -> IO String
getContentType uri = do
  a <- curlHead uri curl_options
  t <- getContentTypeHdr a
  l <- getContentLenHdr a
  return ("File type: [" ++ strip t ++ "] " ++ l )
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

-- | Download contents of a URL.
getContent :: String -> IO String
getContent = getContent_ []

getContent_ :: [CurlOption] -> String -> IO String
getContent_ opts uri = do
  (_,c) <- curlGetString uri (curl_options ++ opts)
  return c

-- | 'curlGetString' performs the same request as 'curlGet', but 
-- returns the response body as a Haskell string.
curlGetString :: URLString
              -> [CurlOption]
              -> IO (CurlCode, String)
curlGetString url opts = initialize >>= \ h -> do
  ref <- newIORef []
   -- Note: later options may (and should, probably) override these defaults.
  setopt h (CurlFailOnError True)
  setDefaultSSLOpts h url
  setopt h (CurlURL url)
  setopt h (CurlWriteFunction (gatherOutput ref))
  mapM_ (setopt h) opts
  rc <- perform h
  lss <- readIORef ref
  return (rc, concat $ reverse lss)


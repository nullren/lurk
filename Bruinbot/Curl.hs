import Control.Monad.Reader
import Network.Curl
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Codec.Binary.UTF8.String


url = "https://bitbucket.com"

maxTitleLength :: Int
maxTitleLength = 80

maxRedirectFollow :: Long
maxRedirectFollow = 5

curl_options :: [CurlOption]
curl_options = [CurlFollowLocation True, CurlMaxRedirs maxRedirectFollow, CurlHeader True]

extractTitle :: String -> Maybe String
extractTitle = content . tags . decodeString where
  tags = closing . opening . canonicalizeTags . parseTags
  opening = dropWhile (not . tagOpenLit "title" (const True))
  closing = takeWhile (not . tagCloseLit "title")
  content = maybeText . format . innerText
  format = unwords . words
  maybeText [] = Nothing
  maybeText t = Just ("Title: " ++ take maxTitleLength (encodeString t))

{-
-- print something like: [image/text] and file size
getHdrDeets :: CurlResponse -> Maybe String
getHdrDeets r = do
  
  case (findHeader HdrContentType r) of
    Nothing -> fail "Given URL has no content type."
    Just ct -> case (findHeader HdrContentLength r) of
      Nothing -> fail "Given URL has no content."
      Just cl -> return ("[" ++ ct ++ "] " ++ cl)
-}

main = do
  resp <- liftIO $ withCurlDo $ curlGetResponse_ url curl_options
  putStrLn (show $ respHeaders resp)

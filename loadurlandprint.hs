import Network.HTTP
import Network.Browser
import Text.HTML.TagSoup

main = do
  title <- getUrlTitle "http://tinyurl.com/8cp69mv"
  putStrLn title

getUrlTitle url = do
  (_, rsp) <- browse $ do
                --setErrHandler $ const (return ())
                --setOutHandler $ const (return ())
                setAllowRedirects True
                request $ getRequest url
  let tags = parseTags $ rspBody rsp
  return (fromTagText (dropWhile (~/= "<title>") tags !! 0))

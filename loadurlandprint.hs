import Data.Char
import Data.List
import Network.HTTP
import Text.HTML.TagSoup


main = printPageTitle "http://imgur.com"

printPageTitle :: String -> IO ()
printPageTitle url = do
    let openURL x = getResponseBody =<< simpleHTTP (getRequest x)
    tags <- fmap parseTags $ openURL url
    let title = fromTagText (dropWhile (~/= "<title>") tags !! 1)
    putStrLn title

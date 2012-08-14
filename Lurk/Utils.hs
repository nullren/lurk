module Lurk.Utils (
  getUrls,
  prettySize
) where

import Data.List

-- just something stupid to fetch a list of urls in a string
getUrls :: String -> [String]
getUrls s = 
  let http = filter (\x -> "http://" `isPrefixOf` x) $ words s
      https = filter (\x -> "https://" `isPrefixOf` x) $ words s
  in http ++ https

-- something to make numbers have pretty things in them
prettySize :: Integer -> String
prettySize = show

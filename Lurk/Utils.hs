module Lurk.Utils (
  getUrls,
  prettySize
) where

import Data.List
import Data.Char

-- just something stupid to fetch a list of urls in a string
getUrls :: String -> [String]
getUrls = filter crit . words where
  crit = theshits . low
  theshits x = ((isPrefixOf "http://") x && ((>7) . length) x)
             ||((isPrefixOf "https://") x && ((>8) . length) x)
  low = map toLower

-- something to make numbers have pretty things in them
prettySize :: Integer -> String
prettySize = show

module Lurk.Utils (
  getUrls,
  head',
  prettySize
) where

import Data.List
import Data.Char
import Text.Printf

-- just something stupid to fetch a list of urls in a string
getUrls :: String -> [String]
getUrls = filter crit . words where
  crit = theshits . low
  theshits x = ((isPrefixOf "http://") x && ((>7) . length) x)
             ||((isPrefixOf "https://") x && ((>8) . length) x)
  low = map toLower

head' [] = []
head' s = head s

-- something to make numbers have pretty things in them
prettySize :: (Real a, Show a) => a -> String
prettySize x 
  | x > 1024^3 = printf "%.1f GB\n" (d x (1024^3)) 
  | x > 1024^2 = printf "%.1f MB\n" (d x (1024^2))
  | x > 1024   = printf "%.1f KB\n" (d x 1024) 
  | otherwise  = show x
  where
    d :: (PrintfArg a, Fractional a, Real a1, Real a2) => a1 -> a2 -> a
    d z y = (realToFrac z)/(realToFrac y)


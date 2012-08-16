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
prettySize :: Integer -> String
prettySize x 
  | x > 1024^3 = printf "%.1fG" (d x (1024^3)) 
  | x > 1024^2 = printf "%.0fM" (d x (1024^2))
  | x > 1024   = printf "%.0fK" (d x 1024) 
  | otherwise  = show x
  where
    d :: Integer -> Integer -> Double
    d z y = (realToFrac z)/(realToFrac y)

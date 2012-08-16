module Lurk.Utils 
  ( getUrls
  , head'
  , prettySize
  , humanReadable
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
prettySize x = humanReadable (realToFrac x) 1024 0

humanReadable :: Double -> Double -> Integer -> String
humanReadable num base power
  | num < 0    = "Negative file size!"
  | power > 4  = "Fucking huge!"
  | num > base = humanReadable (num / base) base (power + 1)
  | otherwise  = printf pstr num (suffix power)
  where
    pstr = if num > 10 then "%.0f%s" else "%.1f%s"
    suffix 0 = ""
    suffix 1 = "K"
    suffix 2 = "M"
    suffix 3 = "G"
    suffix 4 = "T"
    suffix _ = "?"

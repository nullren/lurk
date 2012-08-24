module Lurk.Utils where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import System.Random
import Text.Printf

-- | Just something stupid to fetch a list of urls in a string.
getUrls :: String -> [String]
getUrls = filter crit . words where
  crit = theshits . low
  theshits x = (isPrefixOf "http://" x && ((>7) . length) x)
             ||(isPrefixOf "https://" x && ((>8) . length) x)
  low = map toLower

head' [] = []
head' s = head s

-- | Convert a number to a humanreadable IEC prefix number.
prettySize :: (Integral a) => a -> String
prettySize x = humanReadable (realToFrac x) 1024 0

-- | Converts a number to a humanreadable format. Can take standard or
-- IEC format (or other arbitrary bases).
humanReadable :: Double -> Double -> Int -> String
humanReadable num base power
  | num < 0    = "Negative file size!"
  | power > 8  = "Fucking huge!"
  | num > base = humanReadable (num / base) base (power + 1)
  | otherwise  = printf pstr num (suffix power)
  where
    pstr = if num > 10 then "%.0f%s" else "%.1f%s"
    suffix 0 = ""
    suffix 1 = "K" -- kilo
    suffix 2 = "M" -- mega
    suffix 3 = "G" -- giga
    suffix 4 = "T" -- tera
    suffix 5 = "P" -- peta
    suffix 6 = "E" -- exa
    suffix 7 = "Z" -- zetta
    suffix 8 = "Y" -- yotta
    suffix _ = "?"

-- | A safe way to get a specific element in a list.
safeIndex :: Int -> [a] -> Maybe a
safeIndex _ []     = Nothing
safeIndex 0 (x:_)  = Just x
safeIndex i (_:xs) = safeIndex (i-1) xs

-- | Pick a random element in a list
pick xs = (xs !!) <$> (randomRIO (0, length xs - 1))

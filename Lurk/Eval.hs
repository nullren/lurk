module Lurk.Eval where

import Lurk.Bot.Config
import Lurk.Bot.IRC
import Lurk.Google
import Lurk.Url
import Lurk.Utils

import Data.List
import Network.TinyURL

import System.Exit

import Control.Monad.Reader

eval :: String -> String -> Net ()

-- quit
eval _    "!quit"                  = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)

-- test
eval c x | "!id " `isPrefixOf` x   = privmsg c (drop 4 x)

-- test action
eval c x | "!slap " `isPrefixOf` x = privmsg c ("\001ACTION slaps " ++ (drop 6 x) ++ " around with a large trout!\001")

-- give google search url
eval c x | "!gs " `isPrefixOf` x   = privmsg c $ getGoogleSearchUrl (drop 4 x)

eval c x | "!gsbi " `isPrefixOf` x = do
  r <- liftIO $ getSbiResults (drop 6 x) 
  mapM_ (\(t,u) -> case u of
    Nothing -> privmsg c t
    Just url -> do
      url' <- liftIO $ tinyURL url
      privmsg c (t ++ " <" ++ url' ++ ">")) r
  --privmsg c $ case r of
   -- Nothing -> "eep nothing found"
    --Just s -> s

-- use google to get some info
eval c x | "!g " `isPrefixOf` x    = do
  r <- liftIO $ getSearchResults (drop 3 x)
  mapM_ (\(t,u) -> case u of
    Nothing -> privmsg c t
    Just url -> do
      url' <- liftIO $ tinyURL url
      privmsg c (t ++ " <" ++ url' ++ ">")) r

-- every message look for URLs to get titles for
eval c x | urls@(_:_) <- getUrls x = mapM_ (\x -> do
  title <- liftIO $ getTitle x 
  privmsg c title 
  ) urls

-- do nothing
eval _    _                        = return () -- ignore everything else

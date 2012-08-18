import Data.List
import Data.Maybe
import Network
import System.IO
import System.Time
import System.Exit
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Control.Exception as E
import Text.Printf 

-- required
import Lurk.Config
import Lurk.Utils
import Lurk.Url
import Network.IRC hiding (privmsg, nick)

-- optional plugins
import Lurk.Google

-- url shortener
import Network.TinyURL
 
--
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the bot's start time.
--
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }
 
--
-- Set up actions to run on start and end, and run the main loop
--
main :: IO ()
main = E.bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = E.catch (runReaderT run st) (\(E.SomeException _) -> return ())
 
--
-- Connect to the server and return the initial bot state
--
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo (server lurkBot) (PortNumber . fromIntegral . port $ lurkBot)
    forkIO (forever (getLine >>= hPrintf h "%s\r\n"))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = E.bracket_
        (printf "Connecting to %s ... " (server lurkBot) >> hFlush stdout)
        (putStrLn "done.")
        a
 
--
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
--
run :: Net ()
run = do
    write "NICK" (nick lurkBot)
    write "USER" ((username lurkBot)++" 0 * :" ++ (realname lurkBot))
    asks socket >>= listen
 
--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` liftIO (hGetLine h)
    liftIO (putStrLn s)
    if ping s then pong s else handle s
  where
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)
 
--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t

handle :: String -> Net ()
handle s = do
  case decode (s++"\r\n") of
    Nothing -> return ()
    Just msg -> case msg_command msg of

      -- end of MOTD
      "376"         -> mapM_ (\x -> write "JOIN" x) (channels lurkBot)

      -- nick taken
      "433"         -> write "NICK" (nick lurkBot ++ "_")

      "PRIVMSG"     -> eval tgt mess where
                         chan = head $ msg_params msg
                         mess = last $ msg_params msg
                         tgt = if (nick lurkBot) `isPrefixOf` chan 
                                 then case msg_prefix msg of
                                   Nothing -> chan
                                   Just n -> getnick n
                                 else chan
                         getnick (NickName s _ _) = s

      -- do nothing
      _             -> return ()

eval :: String -> String -> Net ()

-- quit
eval _    "!quit"                  = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)

-- test
eval c x | "!id " `isPrefixOf` x   = privmsg c (drop 4 x)

-- test action
eval c x | "!slap " `isPrefixOf` x = privmsg c ("\001ACTION slaps " ++ (drop 6 x) ++ " around with a large trout!\001")

-- give google search url
eval c x | "!gs " `isPrefixOf` x   = privmsg c $ getGoogleSearchUrl (drop 4 x)

eval c x | "!gsbi " `isPrefixOf` x = privmsg c $ getGoogleSearchByImageUrl (drop 6 x)

-- use google to get some info
eval c x | "!g " `isPrefixOf` x    = do
                                       r <- liftIO $ getSearchResults (drop 3 x)
                                       mapM_ (\(t,u) -> case u of
                                         Nothing -> privmsg c t
                                         Just url -> do
                                           url' <- liftIO $ tinyURL url
                                           privmsg c (t ++ " <" ++ url' ++ ">")) r

-- every message look for URLs to get titles for
eval c x | urls@(_:_) <- getUrls x = mapM_ (\x -> do {
                                       title <- liftIO $ getTitle x;
                                       privmsg c title; }) urls

-- do nothing
eval _    _                        = return () -- ignore everything else


-- short cut
privmsg :: String -> String -> Net ()
privmsg c s = write "PRIVMSG" (c ++ " :" ++ s)

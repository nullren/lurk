import Data.List
import Data.Maybe
import Network
import System.IO
import System.Time
import System.Exit
import Control.Monad.Reader
import qualified Control.Exception as E
import Text.Printf 

-- required
import Lurk.Config
import Lurk.Url
import Network.IRC hiding (privmsg, nick)

-- optional plugins
import Lurk.Google
 
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
    write "USER" ((nick lurkBot)++" 0 * :avocado butt")
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
    forever a = a >> forever a
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
      "376"         -> mapM_ (\x -> write "JOIN" x) (channels lurkBot)
      "PRIVMSG"     -> eval chan mess where
                         chan = head $ msg_params msg
                         mess = last $ msg_params msg
      _             -> return ()

eval :: String -> String -> Net ()
eval _    "!quit"                  = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval c x | "!id " `isPrefixOf` x   = privmsg c (drop 4 x)
eval c x | "!g " `isPrefixOf` x    = do
                                       r <- liftIO $ getSearchResults_ (drop 3 x)
                                       mapM_ (privmsg c) r
eval c x | urls@(_:_) <- getUrls x = mapM_ (\x -> do {
                                       title <- liftIO $ getTitle x;
                                       privmsg c title; }) urls
eval _    _                        = return () -- ignore everything else

privmsg :: String -> String -> Net ()
privmsg c s = write "PRIVMSG" (c ++ " :" ++ s)

clean :: String -> String
clean = drop 1 . dropWhile (/= ':') . drop 1

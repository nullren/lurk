# TODO
   - everything

# BUILD
A simple, clean IRC bot in Haskell

    $ make
    $ ./lurk

or

    $ runhaskell ./lurk.hs

## DEPENDENCIES
Not sure of all the dependencies, but the following are needed

   - Network.IRC `cabal install irc`
   - Codec.Binary.UTF8.String `cabal install utf8-string`
   - Text.HTML.TagSoup.Match `cabal install tagsoup`
   - Network.Curl `cabal install curl`
   - Network `cabal install network`
   - Network.TinyURL `cabal install tinyurl`


This is a little script to install everything needed to run the bot on
Ubuntu. This was tested on 12.04 and all commands completed without
errors but there were some warnings.

    #!/bin/sh
    sudo apt-get update
    sudo apt-get install libghc-curl-dev libsqlite3-dev
    cabal update
    cabal install irc tinyurl utf8-string tagsoup hdbc hdbc-sqlite3


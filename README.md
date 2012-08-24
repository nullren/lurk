# TODO
   - Make a pretty user interface

# BUILD
A simple, clean IRC bot in Haskell

    $ make
    $ ./lurk

or

    $ runhaskell ./lurk.hs

## DEPENDENCIES
Not sure of all the dependencies, but the following are needed

   - Codec.Binary.UTF8.String
   - Database.HDBC
   - Database.HDBC.Sqlite3
   - Network
   - Network.Curl
   - Network.IRC
   - Network.TinyURL
   - Network.TLS
   - Network.TLS.Extra
   - Text.HTML.TagSoup.Match


This is a little script to install everything needed to run the bot on
Ubuntu. This was tested on 12.04 and all commands completed without
errors but there were some warnings.

    #!/bin/sh
    sudo apt-get update
    sudo apt-get install libghc-curl-dev libsqlite3-dev
    cabal update
    cabal install hdbc
    cabal install hdbc-sqlite3
    cabal install irc
    cabal install tagsoup
    cabal install tinyurl
    cabal install tls
    cabal install tls-extra
    cabal install utf8-string


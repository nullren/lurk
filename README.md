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

Fizzckle
========

Yesod/Twilio webapp for managing finances

Building
========

Install ghc and cabal-install.

Navigate to the source root.

$ cabal update
$ cabal install cabal-dev
$ cabal-dev install yesod-platform
$ make

That should do it.

Running
=======

Run locally with

$ ./FizzWeb --debug

Or for realz with

$ ./FizzWeb example.com port

Implementation
==============

Fizzckle stores its data in the directory from which it's run, which is awesome.

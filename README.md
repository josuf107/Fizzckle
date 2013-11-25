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

Fizzckle stores its data in the directory from which it's run, which is
awesome. It will attempt to create a directory called 'data'. Every
budget gets a directory inside the data directory that contains a budget
file and expense history. Entering an expense writes a new expense file
in data/BUDGETNAME/expenses/timestampedfile and an identical file in
data/BUDGETNAME/current/timestampedfile.

Unfortunately, Fizzckle does not keep time automatically. When you wish
for the current budget cycle to be cleared, you must manually clear out
the 'current' directory from the budget folder. The reason for this is
that I sometimes neglect to enter expenses in the budget period, so I
don't want it to advance automatically. One remedy would be to add a
control to the web interface to advance the budget period.

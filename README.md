Fizzckle
========

Yesod/Twilio webapp for managing finances

Building
========

Install ghc and cabal-install.

Navigate to the source root.

```
$ cabal update
$ cabal install cabal-dev
$ cabal-dev install yesod-platform
$ make
```

That should do it.

Running
=======

Run locally with

```
$ ./FizzWeb --debug
```

Or for realz with

```
$ ./FizzWeb example.com port
```

Usage
=====

Fizzckle combines a web interface and a text message interface. Navigate
to /budgets in your browser. Enter budget categories and their values
using the forms. The plus/minus arrows add or subtract $5. To change the
value of a budget use those arrows or simply re-enter it. To remove a
budget category just re-enter it with a value of zero (Awesome!).
Clicking a budget name in the table displays the expense report for that
budget.

The text messaging interface is a bit more complicated, but not too bad. You'll
need to set up Twilio to point a phone number to the webapp. Once you've done
that you can text the following commands:

* `food` -> Fizzckle responds with the balance for the budget "food"
* `5.25 food subway after swim practice` -> Fizzckle records a $5.25
  expense for the budget "food" with the description "subway after swim
  practice" and responds with the new balance
* `@recent food` -> Fizzckle responds with a list of recent expenses for
  the budget "food"
* `@budgets` -> Fizzckle responds with a list of active budgets
* `@budget food 100 weekly` -> Fizzckle sets or creates the budget
  "food" to be $100 weekly (also allows "yearly" and "monthly")
* `@budget food 450` -> Fizzckle sets or creates the budget "food" to be
  $450 monthly
* `? food subway after swim practice` -> Fizzckle records a promise.
  This feature allows you to record that you intend to spend money on
  something but don't know how much yet
* `* food 5.63` -> Fizzckle fulfills your promise by completing the
  entry with the value $5.63

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

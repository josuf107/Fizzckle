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
$ cabal-dev install
```

That should do it. The last step will take a long time.

Running
=======

Run locally with

```
$ cabal-dev/bin/FizzWeb --debug
```

Or for realz with

```
$ cabal-dev/bin/FizzWeb example.com port
```

For Those New to Webapps
------------------------

You'll need to have a dedicated IP address for this to work properly.

Options:

*   Use something like
    [dynamicdns](https://www.dnsdynamic.org/signup.php). This option is
    free and pretty simple, but you'll have to leave your computer on
    all the time and you might need to do port forwarding in your
    router. Plus the domain is probably not going to be the prettiest.
    Also, I've only ever run Fizzckle on Linux.
*   Use a VPS service like
    [linode](https://library.linode.com/getting-started). I use Linode
    to host a number of webapps including Fizzckle, and while it does
    cost some money it's a great solution. You'll still need a domain.
    You could use something like dynamicdns to point to the VPS, or you
    could purchase a domain.

Usage
=====

Fizzckle combines a web interface and a text message interface. Navigate
to /budgets in your browser. Enter budget categories and their values
using the forms. The plus/minus arrows add or subtract $5. To change the
value of a budget use those arrows or simply re-enter it. To remove a
budget category just re-enter it with a value of zero (Awesome!).
Clicking a budget name in the table displays the expense report for that
budget.

The text messaging interface is a bit more complicated, but not too bad.
You'll need to set up [Twilio](https://www.twilio.com) to point a phone
number to the webapp at /dash/fizz (I just use a trial Twilio account
for this since it's just me and one other person using it, but you'll
need to verify every phone number you want to be able to text from).
Once you've done that you can text the following commands:

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

Fizzckle does not keep time automatically. When you wish for the current
budget cycle to be cleared, you must click "tick" in the web interface.
The reason for this is that I sometimes neglect to enter expenses in the
budget period, so I don't want it to advance automatically.

When calculating the monthly value for the budget planner at /budgets,
Fizzckle assumes 4.5 weeks per month. The reason for this is that
Fizzckle allows you to tick weekly budgets at your own disgression. I
have some weekly budgets that I tick on Saturday and some that I tick on
Monday. Therefore, Fizzckle does not know how many ticks will occur in a
given month for a weekly budget. I use 4.5 because it's easy to
remember, and,  over time, it's an overestimate (it equates to a 378 day
year). For a non-leap year, the number of weeks per month is more like
365 / 12 / 7 = 4.3452. Anyway, upshot of all this is that if you have a
weekly budget of $100 for clothes (no judgement) and make $500 a month,
at the bottom of the budgets page it will say "Total $450 of $500
leaving $50.00."

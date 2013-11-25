FizzWeb : FizzWeb.hs Fizz.hs Fizz/Core.hs Fizz/Store.hs Fizz/Utils.hs budgets.cassius budgets.hamlet budgets.julius expenses.hamlet expense.hamlet
	ghc -package-db cabal-dev/packages-7.6.3.conf -O2 --make FizzWeb -main-is FizzWeb -o FizzWeb

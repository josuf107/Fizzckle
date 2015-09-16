import qualified Fizz.Core as FC
import qualified Fizz.Store as FS
import Data.Time
import Control.Applicative

data ExpenseEntry
    = ExpenseEntry
        { getExpenseTag :: [String]
        , getExpenseCategory :: FC.Category
        , getExpenseValue :: Double
        , getExpenseDescription :: String
        , getExpenseTime :: LocalTime
        } deriving(Show, Read, Eq)

toNew :: (FC.ExpenseEntry -> FC.Entry) -> ExpenseEntry -> FC.Timestamped FC.Entry
toNew kind e = (getExpenseTime e, kind $ FC.newExpenseEntry (getExpenseCategory e) (getExpenseValue e) (getExpenseDescription e))

oldExpenses :: IO [ExpenseEntry]
oldExpenses = fmap read . lines <$> readFile "testdata/expenses/all"

oldSavings :: IO [ExpenseEntry]
oldSavings = fmap read . lines <$> readFile "testdata/savings/all"

module Fizz.Core
    ( Entry(..)
    , Journal
    , Timestamped
        , getTimestamp
    , Category
        , mkCategory
    , Budget
    , Expenses
    , ExpenseEntry
        , getExpenseTag
        , getExpenseCategory
        , getExpenseValue
        , getExpenseDescription
        , newExpenseEntry
    , BudgetEntry
        , getBudgetValue
        , getBudgetCategory
        , getBudgetFrequency
        , getBudgetType
        , newBudgetEntry
    , BudgetType(..)
    , Frequency(..)
    , recentExpenseReport
    , printBudgetReport
    , printCategory
    , budgetCategory
    , spendCategory
    , mostRecentBudgets
    , getMonthlyValue
    , totalSpent
    , totalEarned
    , totalSaved
    , totalAvailableSavings
    , totalRealized
    , totalDisposable
    , totalBudgeted
    , isRealize
    , isSpend
    , isSave
    , isBudget
    , isEarn
    , getCategory
    , categories
    , next
    )
where

import Fizz.Utils

import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Time
import Data.Time.Calendar.WeekDate

type Timestamped a = (LocalTime, a)
type Journal = [Timestamped Entry]

data Entry
    = Budget BudgetEntry
    | Spend ExpenseEntry
    | Save ExpenseEntry
    | Earn ExpenseEntry
    | Realize ExpenseEntry
    | Undo Entry
    | Redo Entry
    deriving (Show, Read, Eq)

type Tag = String

data Category   = Uncategorized
                | Category String deriving (Show, Read, Eq, Ord)

type Budget = [(Category, BudgetEntry)]

type Expenses = [ExpenseEntry]

data Frequency  = Weekly
                | Monthly
                | Yearly deriving(Show, Read, Eq)

class Cycle a where
    next :: a -> a

instance Cycle Frequency where
    next Weekly = Monthly
    next Monthly = Yearly
    next Yearly = Weekly

data BudgetType
    = Expense
    | Savings
    deriving (Show, Read, Eq)

data BudgetEntry
    = BudgetEntry
        { getBudgetValue :: Double
        , getBudgetCategory :: Category
        , getBudgetFrequency :: Frequency
        , getBudgetType :: BudgetType
        } deriving(Show, Read, Eq)

data ExpenseEntry
    = ExpenseEntry
        { getExpenseTag :: [Tag]
        , getExpenseCategory :: Category
        , getExpenseValue :: Double
        , getExpenseDescription :: String
        } deriving(Show, Read, Eq)

type Print a = a -> String

printTimestampedEntry :: Print (Timestamped ExpenseEntry)
printTimestampedEntry (t, e) = "\t"
    ++ (show . localDay $ t)
    ++ ": $"
    ++ (show
        . (/100.0)
        . fromIntegerToDouble
        . round
        . (*100.0)
        . getExpenseValue $ e)
    ++ " --- "
    ++ getExpenseDescription e

printCategory :: Print Category
printCategory Uncategorized = "UNCATEGORIZED"
printCategory (Category c) = c

getTimestamp :: Timestamped a -> LocalTime
getTimestamp = fst

mkCategory :: String -> Category
mkCategory c    | isEmpty c = Uncategorized
                | otherwise = Category . normalizeCategory $ c
                    where
                        normalizeCategory :: String -> String
                        normalizeCategory = fmap toUpper
                            . filter ((||) <$> isLetter <*> isDigit)

defaultEntry :: ExpenseEntry
defaultEntry = ExpenseEntry
    { getExpenseTag = []
    , getExpenseCategory = Uncategorized
    , getExpenseValue = 0
    , getExpenseDescription = ""
    }

newBudgetEntry :: Category -> Double -> Frequency -> BudgetType -> BudgetEntry
newBudgetEntry c v f t = BudgetEntry v c f t

newExpenseEntry :: Category -> Double -> String -> ExpenseEntry
newExpenseEntry c v d = defaultEntry
        { getExpenseCategory = c
        , getExpenseValue = v
        , getExpenseDescription = d
        }

getMonthlyValue :: BudgetEntry -> Double
getMonthlyValue be =
    case getBudgetFrequency be of
        Weekly -> 4.5 * getBudgetValue be
        Monthly -> getBudgetValue be
        Yearly -> getBudgetValue be / 12

nearest :: Frequency -> IO Int
nearest f = do
    time <- getTime
    let (year, _, dow) = toWeekDate . localDay $ time
    let (_, month, dom) = toGregorian . localDay $ time
    let doy = toDoy year month dom
    return $ case f of
        Weekly -> (6 - dow) `mod` 7
        Monthly -> gregorianMonthLength year month - dom
        Yearly -> 365 - doy
    where
        toDoy :: Integer -> Int -> Int -> Int
        toDoy year month dom = (+dom)
            . sum
            . fmap (gregorianMonthLength year) $ [1..month - 1]

total :: (Entry -> Bool) -> (Entry -> ExpenseEntry) -> Journal -> Double
total test extract
    = sum
    . fmap (getExpenseValue . extract)
    . filter test
    . fmap snd

totalEarned :: Journal -> Double
totalEarned = total isEarn (\(Earn e) -> e)

totalSpent :: Journal -> Double
totalSpent = total isSpend (\(Spend e) -> e)

totalSaved :: Journal -> Double
totalSaved = total isSave (\(Save e) -> e)

totalAvailableSavings :: Journal -> Double
totalAvailableSavings j = totalSaved j - totalRealized j

totalRealized :: Journal -> Double
totalRealized = total isRealize (\(Realize e) -> e)

totalDisposable :: Journal -> Double
totalDisposable j = totalEarned j - totalSpent j - totalAvailableSavings j

totalBudgeted :: Journal -> Double
totalBudgeted = sum
    . fmap (\(Budget be) -> getBudgetValue be)
    . filter isBudget
    . fmap snd

recentExpenseReport :: Category -> Journal -> String
recentExpenseReport c
    = unlines
    . fmap (\(time, Spend e) -> printTimestampedEntry (time, e))
    . take 10
    . reverse
    . sortBy (on compare fst)
    . filter (\(_, Spend e) -> (==c) . getExpenseCategory $ e)
    . filter (isSpend . snd)

isSpend :: Entry -> Bool
isSpend (Spend _) = True
isSpend _ = False

isBudget :: Entry -> Bool
isBudget (Budget _) = True
isBudget _ = False

isEarn :: Entry -> Bool
isEarn (Earn _) = True
isEarn _ = False

isSave :: Entry -> Bool
isSave (Save _) = True
isSave _ = False

isRealize :: Entry -> Bool
isRealize (Realize _) = True
isRealize _ = False

budgetCategory :: Category -> Entry -> Bool
budgetCategory c (Budget be) = getBudgetCategory be == c
budgetCategory _ _ = False

spendCategory :: Category -> Entry -> Bool
spendCategory c (Spend e) = getExpenseCategory e == c
spendCategory _ _ = False

categories :: Journal -> [(Category, Journal)]
categories
    = M.toAscList
    . M.fromListWith (++)
    . fmap (\e -> (getCategory . snd $ e, [e]))

getCategory :: Entry -> Category
getCategory (Budget be) = getBudgetCategory be
getCategory (Spend e) = getExpenseCategory e
getCategory (Earn e) = getExpenseCategory e
getCategory (Save e) = getExpenseCategory e
getCategory (Realize e) = getExpenseCategory e
getCategory (Undo entry) = getCategory entry
getCategory (Redo entry) = getCategory entry

mostRecentBudgets :: Journal -> Budget
mostRecentBudgets
    = fmap (\(_, be) -> (getBudgetCategory be, be))
    . fmap (maximumBy (on compare fst)) -- take latest in each group
    . groupBy (on (==) (getBudgetCategory . snd)) -- group by category
    . sortBy (on compare (getBudgetCategory . snd))
    . fmap (\(t, Budget be) -> (t, be)) -- get budget entries
    . filter (isBudget . snd)

printBudgetReport :: BudgetEntry -> Expenses -> IO String
printBudgetReport be es = do
    let freq = getBudgetFrequency be
    daysRemaining <- show <$> nearest freq
    let bs = sum . fmap getExpenseValue $ es
    let bv = getBudgetValue be
    let bb = bv - bs
    return $ "Of "
        ++ showDollars bv
        ++ " budgeted we've spent "
        ++ showDollars bs
        ++", leaving "
        ++ showDollars bb
        ++ " for the next " ++ daysRemaining ++ " days."

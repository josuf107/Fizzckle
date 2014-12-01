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
    , Frequency
    , recentExpenseReport
    )
where

import Fizz.Utils

import Control.Applicative
import Data.Char
import Data.Function
import Data.List
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
                | Category String deriving (Show, Read, Eq)

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

printBudget :: Budget -> String
printBudget budget =
    let
        body = fmap (show . fst) budget
        total = sum . fmap (getMonthlyValue . snd) $ budget
    in
        "Budgets "
        ++ intercalate ", " body
        ++ " total "
        ++ showDollars total

printLongBudget :: Budget -> String
printLongBudget budget =
    let
        value = showDollars . getBudgetValue
        freq = show . getBudgetFrequency
        showLine (t, b) =
            "\t"
            ++ value b ++ " (" ++ freq  b ++ ") "
            ++ show t
        body = fmap showLine budget
        total = sum . fmap (getMonthlyValue . snd) $ budget
    in
        "Budget report:\n"
        ++ intercalate "\n" body
        ++ "\nTotal: "
        ++ showDollars total

budgetTotal :: Budget -> Double
budgetTotal = fromIntegerToDouble
    . round . sum . fmap (getBudgetValue . snd)

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

totalSpent :: Expenses -> Double
totalSpent = sum . fmap getExpenseValue

recentExpenseReport :: Category -> Journal -> String
recentExpenseReport c
    = unlines
    . fmap (\(time, Spend e) -> printTimestampedEntry (time, e))
    . take 10
    . reverse
    . sortBy (on compare fst)
    . filter (\(_, Spend e) -> (==c) . getExpenseCategory $ e)
    . filter (isSpent . snd)

isSpent :: Entry -> Bool
isSpent (Spend _) = True
isSpent _ = False

isBudget :: Entry -> Bool
isBudget (Budget _) = True
isBudget _ = False

printBudgetReport :: BudgetEntry -> Expenses -> IO String
printBudgetReport be es = do
    let freq = getBudgetFrequency be
    daysRemaining <- show <$> nearest freq
    let bs = totalSpent es
    let bv = getBudgetValue be
    let bb = bv - bs
    return $ "Of "
        ++ showDollars bv
        ++ " budgeted we've spent "
        ++ showDollars bs
        ++", leaving "
        ++ showDollars bb
        ++ " for the next " ++ daysRemaining ++ " days."

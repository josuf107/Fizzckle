module Fizz.Core
    ( Tag
    , Category
    , mkCategory
    , Budget
    , Expenses
    , Frequency (Weekly, Monthly, Yearly)
    , BudgetEntry
    , BudgetType(..)
    , getBudgetCategory
    , getBudgetValue
    , getBudgetFrequency
    , getBudgetType
    , newBudgetEntry
    , ExpenseEntry
    , getExpenseValue
    , getExpenseCategory
    , getExpenseTag
    , getExpenseDescription
    , getExpenseTime
    , newExpenseEntry
    , newPromise
    , next
    , Action    ( Error
                , Fulfill
                , Future
                , Promise
                , BudgetReport
                , BudgetsReport
                , RecentExpenseReport
                , EnterBudget
                , EnterExpense
                )
    , getExpenseYear
    , getExpenseMonth
    , getExpenseDom
    , getExpenseWeek
    , getExpenseDow
    , getExpenseDiffTime
    , printBudget
    , printLongBudget
    , budgetTotal
    , getMonthlyValue
    , printBudgetReport
    , pretty
    , future
    , showDollars
    )
where

import Fizz.Utils

import Control.Applicative
import Data.Char
import Data.List
import Data.Time
import Data.Time.Calendar.WeekDate
import Text.Printf

type Journal = [(LocalTime, Entry)]

data Entry
    = Budget BudgetEntry
    | Spend ExpenseEntry
    | Save ExpenseEntry
    | Earn ExpenseEntry
    | Realize ExpenseEntry
    | Undo Entry
    | Redo Entry
    deriving (Show, Eq)

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
        , getExpenseTime :: LocalTime
        } deriving(Show, Read, Eq)

data WeekDate
    = WeekDate
        { getWeekDateYear :: Integer
        , getWeekDateWeek :: Int
        , getWeekDateDow :: Int
        } deriving(Show, Read)

data MonthDate
    = MonthDate
        { getMonthDateMonth :: Int
        , getMonthDateDom :: Int
        } deriving(Show, Read)

data Action
    = Error String
    | Future Double Double Double Integer
    | BudgetReport Category
    | BudgetsReport
    | Promise ExpenseEntry
    | Fulfill ExpenseEntry
    | RecentExpenseReport Category
    | EnterBudget BudgetEntry
    | EnterExpense ExpenseEntry deriving(Show)

class Pretty a where
    pretty :: a -> String

instance Pretty ExpenseEntry where
    pretty e = "\t"
        ++ (show . localDay . getExpenseTime $ e)
        ++ ": $"
        ++ (show
            . (/100.0)
            . fromIntegerToDouble
            . round
            . (*100.0)
            . getExpenseValue $ e)
        ++ " --- "
        ++ getExpenseDescription e

instance Pretty Category where
    pretty Uncategorized = "UNCATEGORIZED"
    pretty (Category c) = c

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
    , getExpenseTime = LocalTime (ModifiedJulianDay 1) midnight
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
        body = fmap (pretty . fst) budget
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
            ++ pretty t
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

newBudgetEntry :: Category -> Double -> Frequency -> BudgetType -> BudgetEntry
newBudgetEntry c v f t = BudgetEntry v c f t

newPromise :: Category -> String -> ExpenseEntry
newPromise c = tagExpense ".promise" . newExpenseEntry c 0

tagExpense :: Tag -> ExpenseEntry -> ExpenseEntry
tagExpense t e = e { getExpenseTag = newTags }
    where
        newTags :: [Tag]
        newTags = nub $ t : (getExpenseTag e)

newExpenseEntry :: Category -> Double -> String -> ExpenseEntry
newExpenseEntry c v d = defaultEntry
        { getExpenseCategory = c
        , getExpenseValue = v
        , getExpenseDescription = d
        }

getExpenseDom :: ExpenseEntry -> Int
getExpenseDom = getMonthDateDom . getExpenseMonthDate

getExpenseDow :: ExpenseEntry -> Int
getExpenseDow = getWeekDateDow . getExpenseWeekDate

getExpenseMonth :: ExpenseEntry -> Int
getExpenseMonth = getMonthDateMonth . getExpenseMonthDate

getExpenseWeek :: ExpenseEntry -> Int
getExpenseWeek = getWeekDateWeek . getExpenseWeekDate

getExpenseYear :: ExpenseEntry -> Integer
getExpenseYear = getWeekDateYear . getExpenseWeekDate

getExpenseMonthDate :: ExpenseEntry -> MonthDate
getExpenseMonthDate e =
    let
        (_, month, dom) = toGregorian . localDay . getExpenseTime $ e
    in
        MonthDate month dom

getExpenseWeekDate :: ExpenseEntry -> WeekDate
getExpenseWeekDate e =
    let
        (year, week, dow) = toWeekDate . localDay . getExpenseTime $ e
    in
        WeekDate year week dow

getExpenseDiffTime :: ExpenseEntry -> DiffTime
getExpenseDiffTime = timeOfDayToTime
    . localTimeOfDay
    . getExpenseTime

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

isEmpty :: String -> Bool
isEmpty = (=="") . filter (not . flip elem whiteChars)

showDollars :: Double -> String
showDollars d
    | d >= 0 = printf "$%.2f" d
    | otherwise = printf "-$%.2f" (abs d)

future :: Double -> Double -> Double -> Integer -> Double
future principle _ _ 0 = principle
future principle increment rate periods =
    future ((principle + increment) * rate) increment rate (periods - 1)

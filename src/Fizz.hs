module Fizz where

import Fizz.Core
import Fizz.Store
import Fizz.Utils

import Control.Applicative
import Control.Monad (void)
import qualified Text.ParserCombinators.Parsec as PC

data Action
    = Error String
    | BudgetReport Category
    | RecentExpenseReport Category
    | EnterSaving ExpenseEntry
    | EnterEarning ExpenseEntry
    | EnterBudget BudgetEntry
    | EnterExpense ExpenseEntry deriving(Show)

parseFizz :: String -> Action
parseFizz = either (Error . show) id
    . PC.parse fizzParser "text"

doFizz :: Action -> IO String
doFizz (Error s) = return $ "Error: " ++ s
doFizz (EnterSaving e) = do
    save e
    doFizz . BudgetReport . getExpenseCategory $ e
doFizz (EnterExpense e) = do
    spend e
    doFizz . BudgetReport . getExpenseCategory $ e
doFizz (EnterEarning e) = do
    earn e
    doFizz . BudgetReport . getExpenseCategory $ e
doFizz (RecentExpenseReport c) = do
    recentExpenseReport c <$> queryBack 30
doFizz (EnterBudget b) = do
    budget b
    return "New budget created"
doFizz (BudgetReport t) = do
    mbe <- findEntry (budgetCategory t)
    journal <- queryUntil (budgetCategory t)
    case mbe of
        Just (_, Budget be)
            -> printBudgetReport be
            . fmap (\(Spend e) -> e)
            . filter (spendCategory t)
            . fmap snd
            $ journal
        _ -> doFizz . Error $ 
            "\"" ++ printCategory t ++ "\" is not a budget item"

fizzParser :: PC.GenParser Char st Action
fizzParser = PC.choice [emptyParser
    , actionParser
    , entryParser
    , queryParser]

nonwhitespace:: PC.GenParser Char st String
nonwhitespace = PC.many (PC.noneOf whiteChars)

whitespace:: PC.GenParser Char st ()
whitespace = void $ PC.many (PC.oneOf whiteChars)

emptyParser :: PC.GenParser Char st Action
emptyParser = do
    whitespace
    PC.eof
    return (Error "Empty message")

entryParser :: PC.GenParser Char st Action
entryParser = do
    e <- expenseEntryParser
    return . EnterExpense $ e

queryParser :: PC.GenParser Char st Action
queryParser = do
    whitespace
    c <- nonwhitespace
    return $ BudgetReport (mkCategory c)

actionParser :: PC.GenParser Char st Action
actionParser = do
    whitespace
    void $ PC.char '@'
    PC.choice [PC.try recentParser
        , PC.try budgetEntryParser
        , PC.try saveEntryParser
        , PC.try earnEntryParser
        , return (Error "Unrecognized @command")]

recentParser :: PC.GenParser Char st Action
recentParser = do
    void $ PC.string "recent"
    whitespace
    c <- nonwhitespace
    return $ RecentExpenseReport (mkCategory c)

budgetEntryParser :: PC.GenParser Char st Action
budgetEntryParser = do
    void $ PC.string "budget"
    whitespace
    c <- mkCategory <$> nonwhitespace
    whitespace
    v <- read <$> PC.many1 (PC.digit <|> PC.char '.')
    whitespace
    f <- frequencyParser
    return . EnterBudget $ newBudgetEntry c v f Expense

saveEntryParser :: PC.GenParser Char st Action
saveEntryParser = do
    void $ PC.string "save"
    e <- expenseEntryParser
    return . EnterSaving $ e

earnEntryParser :: PC.GenParser Char st Action
earnEntryParser = do
    void $ PC.string "earn"
    e <- expenseEntryParser
    return . EnterEarning $ e

expenseEntryParser :: PC.GenParser Char st ExpenseEntry
expenseEntryParser = do
    whitespace
    v <- read <$> PC.many1 (PC.digit <|> PC.char '.')
    whitespace
    c <- mkCategory <$> nonwhitespace
    whitespace
    d <- PC.many1 PC.anyChar
    return $ newExpenseEntry c v d

frequencyParser :: PC.GenParser Char st Frequency
frequencyParser =
    PC.choice [PC.string "weekly" >> return Weekly
        , PC.string "Weekly" >> return Weekly
        , PC.string "monthly" >> return Monthly
        , PC.string "Monthly" >> return Monthly
        , PC.string "yearly" >> return Yearly
        , PC.string "Yearly" >> return Yearly
        , return Monthly]

module Fizz where

import Fizz.Core
import Fizz.Store
import Fizz.Utils

import Control.Applicative
import Control.Monad (void)
import qualified Text.ParserCombinators.Parsec as PC

parseFizz :: String -> Action
parseFizz = either (Error . show) id
    . PC.parse fizzParser "text"

doFizz :: Action -> IO String
doFizz (Error s) = return $ "Error: " ++ s
doFizz (EnterExpense e) = do
    void $ writeExpenseEntry e
    doFizz . BudgetReport . getExpenseCategory $ e
doFizz (Promise e) = do
    void $ writePromise e
    return $ "Promised for " ++ (show . getExpenseCategory $ e)
doFizz (Fulfill e) = do
    promises <- readPromises (getExpenseCategory $ e)
    case promises of
        (p:_) -> do
            fulfillPromise (p { getExpenseValue = getExpenseValue e} )
            doFizz . BudgetReport . getExpenseCategory $ e
        _ -> return "No promise for that category"
doFizz (RecentExpenseReport c) = recentExpenseReport c
doFizz BudgetsReport = do
    b <- readBudget
    return $ printBudget b
doFizz (EnterBudget b) = do
    writeBudgetEntry b
    return "New budget created"
doFizz (BudgetReport t) = do
    mbe <- getBudgetEntry t
    case mbe of
        Nothing -> doFizz . Error $ 
            "\"" ++ pretty t ++ "\" is not a budget item"
        Just be -> do
            es <- readCurrentExpenses (getBudgetCategory be)
            printBudgetReport be es
doFizz (Future p i r ps) = return . show $ future p i r ps

fizzParser :: PC.GenParser Char st Action
fizzParser = PC.choice [emptyParser
    , actionParser
    , promiseParser
    , fulfillParser
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
    whitespace
    v <- read <$> PC.many1 (PC.digit <|> PC.char '.')
    whitespace
    c <- mkCategory <$> nonwhitespace
    whitespace
    d <- PC.many1 PC.anyChar
    return . EnterExpense $ newExpenseEntry c v d

queryParser :: PC.GenParser Char st Action
queryParser = do
    whitespace
    c <- nonwhitespace
    return $ BudgetReport (mkCategory c)

promiseParser :: PC.GenParser Char st Action
promiseParser = do
    whitespace
    void $ PC.char '?'
    whitespace
    c <- mkCategory <$> nonwhitespace
    whitespace
    d <- PC.many1 PC.anyChar
    return . Promise $ newPromise c d

fulfillParser :: PC.GenParser Char st Action
fulfillParser = do
    whitespace
    void $ PC.char '*'
    whitespace
    c <- mkCategory <$> nonwhitespace
    whitespace
    v <- read <$> PC.many1 (PC.digit <|> PC.char '.')
    return . Fulfill $ newExpenseEntry c v ""

actionParser :: PC.GenParser Char st Action
actionParser = do
    whitespace
    void $ PC.char '@'
    PC.choice [PC.try recentParser
        , PC.try budgetEntryParser
        , PC.try budgetsParser
        , return (Error "Unrecognized @command")]

recentParser :: PC.GenParser Char st Action
recentParser = do
    void $ PC.string "recent"
    whitespace
    c <- nonwhitespace
    return $ RecentExpenseReport (mkCategory c)

budgetsParser :: PC.GenParser Char st Action
budgetsParser = do
    void $ PC.string "budgets"
    return BudgetsReport

budgetEntryParser :: PC.GenParser Char st Action
budgetEntryParser = do
    void $ PC.string "budget"
    whitespace
    c <- mkCategory <$> nonwhitespace
    whitespace
    v <- read <$> PC.many1 (PC.digit <|> PC.char '.')
    whitespace
    f <- frequencyParser
    return . EnterBudget $ newBudgetEntry c v f

frequencyParser :: PC.GenParser Char st Frequency
frequencyParser =
    PC.choice [PC.string "weekly" >> return Weekly
        , PC.string "Weekly" >> return Weekly
        , PC.string "monthly" >> return Monthly
        , PC.string "Monthly" >> return Monthly
        , PC.string "yearly" >> return Yearly
        , PC.string "Yearly" >> return Yearly
        , return Monthly]

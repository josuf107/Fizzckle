module Fizz.Store where

import Fizz.Core
import Fizz.Utils

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as BS

dataDir :: FilePath
dataDir = "data"

categoryDir :: Category -> FilePath
categoryDir = (dataDir </>) . pretty

budgetFile :: Category -> FilePath
budgetFile c = categoryDir c </> "budget"

currentExpenseDir :: Category -> FilePath
currentExpenseDir c = categoryDir c </> "current"

expenseDir :: Category -> FilePath
expenseDir c = categoryDir c </> "expenses"

promiseDir :: Category -> FilePath
promiseDir c = categoryDir c </> "promises"

promise :: Category -> FilePath
promise c = promiseDir c </> show c

strictWrite :: Bool -> FilePath -> String -> IO ()
strictWrite create fn s = do
    when create (createDirectoryIfMissing True (takeDirectory fn))
    BS.writeFile fn (BS.pack s)

strictRead :: Bool -> FilePath -> IO (Maybe String)
strictRead create fn = do
    when create (createDirectoryIfMissing True (takeDirectory fn))
    exists <- doesFileExist fn
    if exists
        then Just . BS.unpack <$> BS.readFile fn
        else return Nothing

readCategories :: IO [Category]
readCategories = fmap mkCategory <$> getVisibleContents True dataDir

getVisibleContents :: Bool -> FilePath -> IO [FilePath]
getVisibleContents create d =
    if create then
        do
            createDirectoryIfMissing True d
            fs <- getDirectoryContents d
            return $ getVisible fs
    else
        do
            e <- doesDirectoryExist d
            if e
                then getVisible <$> getDirectoryContents d
                else return []
    where
        getVisible :: [FilePath] -> [FilePath]
        getVisible = filter (not . isPrefixOf ".")

readBudgetEntry :: Category -> IO (Maybe BudgetEntry)
readBudgetEntry c = do
    mbs <- strictRead False (budgetFile c)
    case mbs of
        Just bs ->
            case reads bs of
                [] -> return Nothing
                ((be, _):_) -> return $ Just be
        Nothing -> return Nothing

readBudget :: IO Budget
readBudget = do
    cs <- readCategories
    bes <- sequence $ fmap readBudgetEntry cs
    let maybeBudget = zip cs bes
    let budget = justEntries maybeBudget
    return budget
    where
        justEntries :: [(Category, Maybe BudgetEntry)] ->
            [(Category, BudgetEntry)]
        justEntries = catMaybes . fmap maybeEntry
        maybeEntry :: (Category, Maybe BudgetEntry) ->
            Maybe (Category, BudgetEntry)
        maybeEntry (c, mbe) =
            case mbe of
                Nothing -> Nothing
                Just be -> Just (c, be)

writeBudgetEntry :: BudgetEntry -> IO ()
writeBudgetEntry be = strictWrite True bf (show be)
    where
        bf = budgetFile
            . getBudgetCategory $ be

writeBudget :: Budget -> IO ()
writeBudget = sequence_ . fmap (writeBudgetEntry . snd)

addBudget :: Frequency -> Double -> Category -> IO ()
addBudget f v c = do
    let be = newBudgetEntry c v f
    writeBudgetEntry be

addMonthlyBudget :: Double -> Category -> IO ()
addMonthlyBudget = addBudget Monthly

addWeeklyBudget :: Double -> Category -> IO ()
addWeeklyBudget = addBudget Weekly

recentExpenseReport :: Category -> IO String
recentExpenseReport c = do
    es <- readCurrentExpenses c
    let recents = fmap pretty . take 3 . sortBy timeline $ es
    return $ intercalate "\n" recents
    where
        timeline e1 e2 =
            compare (getExpenseValue e1) (getExpenseValue e2)

writePromise :: ExpenseEntry -> IO Category
writePromise e = do
    strictWrite True (promise c) (show e)
    return c
    where
        c = getExpenseCategory e

readPromises :: Category -> IO [ExpenseEntry]
readPromises c = readExpenses (promiseDir c)

fulfillPromise :: ExpenseEntry -> IO ()
fulfillPromise e = do
    writeExpenseEntry e
    removeFile . promise . getExpenseCategory $ e

writeExpenseEntry :: ExpenseEntry -> IO ()
writeExpenseEntry e = do
    let c = getExpenseCategory e
    t <- getTime
    let e' = e {getExpenseTime = t}
    strictWrite True (primaryPath e') (show e')
    let cur = currentExpenseDir c </> expenseFileName e'
    strictWrite True cur (show e')

tickExpenseEntry :: ExpenseEntry -> IO ()
tickExpenseEntry e = do
    let c = getExpenseCategory e
    let cur = currentExpenseDir c </> expenseFileName e
    removeFile cur

tickExpenseCategory :: Category -> IO ()
tickExpenseCategory c = do
    es <- getVisibleContents False (currentExpenseDir c)
    mapM_ removeFile . fmap (currentExpenseDir c </>) $ es

readExpenses :: FilePath -> IO [ExpenseEntry]
readExpenses d = do
    efs <- getVisibleContents False d
    es <- sequence
        . fmap (strictRead False
            . (d </>)) $ efs
    return . catMaybes . fmap maybeRead . catMaybes $ es

readCurrentExpenses :: Category -> IO [ExpenseEntry]
readCurrentExpenses c = readExpenses (currentExpenseDir c)

readAllExpenses :: Category -> IO [ExpenseEntry]
readAllExpenses c = readExpenses (expenseDir c)

expenseFileName :: ExpenseEntry -> FilePath
expenseFileName e = intercalate "-" parts
    where
        parts :: [String]
        parts =
            [ show $ getExpenseYear e
            , show $ getExpenseMonth e
            , show $ getExpenseDom e
            , show $ getExpenseWeek e
            , show $ getExpenseDow e
            , show $ getExpenseDiffTime e
            ]

primaryPath :: ExpenseEntry -> FilePath
primaryPath e = d </> expenseFileName e
    where
        d :: FilePath
        d = expenseDir . getExpenseCategory $ e

getBudgetEntry :: Category -> IO (Maybe BudgetEntry)
getBudgetEntry c = do
    b <- readBudget
    return $ lookup c b

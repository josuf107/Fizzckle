{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Fizz
import Fizz.Core as Fizz
import qualified Fizz.Store as Fizz
import Fizz.Utils (showDollars, between, getTime)

import Data.Char (toLower)
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import System.Environment (getArgs)
import Text.Cassius
import Text.Julius
import Yesod

data Fizz = Fizz T.Text

main :: IO ()
main = do
    args <- getArgs
    if "--debug" `elem` args
        then debug
        else prod args

prod :: [String] -> IO ()
prod (host:port:_) =
    case reads port of
        ((p,_):_) -> warp p (Fizz . T.pack $ host)
        _ -> error $ "Invalid port " ++ port
prod (host:_) = warp 80 (Fizz . T.pack $ host)
prod _ = error "Must specify host a la FizzWeb example.com 8080"

debug :: IO ()
debug = warp 3000 (Fizz "localhost")

mkYesod "Fizz" [parseRoutes|
/budgets BudgetsR GET
/budgets/#CategoryPiece BudgetR GET PUT DELETE
/savings/#CategoryPiece SavingsR DELETE
/expenses ExpensesR GET
/expenses/#CategoryPiece ExpenseCategoryR GET POST
/dash DashR GET
/dash/fizz FizzR POST
|]

newtype CategoryPiece = CategoryPiece Category deriving (Show, Eq, Read)

wrap :: Category -> CategoryPiece
wrap = CategoryPiece

unwrap :: CategoryPiece -> Category
unwrap (CategoryPiece c) = c

categoryPath :: Category -> T.Text
categoryPath = T.pack . fmap toLower . printCategory

instance PathPiece CategoryPiece where
    toPathPiece = categoryPath . unwrap
    fromPathPiece = Just . wrap . mkCategory . T.unpack

instance Yesod Fizz where
    approot = ApprootMaster (\(Fizz f) -> f)

postFizzR :: Handler RepXml
postFizzR = do
    mBody <- lookupPostParam "Body"
    let body = fromMaybe "" mBody
    $(logDebug) $ "Request: " `T.append` body
    let action = parseFizz . T.unpack $ body
    content <- liftIO . doFizz $ action
    let responseBody = wrapBody . T.pack $ content
    $(logDebug) $ "Response: " `T.append` responseBody
    return . RepXml . toContent $ responseBody
    where
        wrapBody :: T.Text -> T.Text
        wrapBody c = "<Response><Sms>"
            `T.append` c `T.append` "</Sms></Response>"

getBudgetsR :: Handler Html
getBudgetsR = defaultLayout $ do
    setTitle "Fizzckle"
    journal <- liftIO Fizz.loadJournal
    let allBudgets = fmap snd . mostRecentBudgets $ journal
    let activeCategories = fmap getBudgetCategory allBudgets :: [Category]
    let spentEach = fmap (\c
            -> totalSpent
            . filter (spendCategory c . snd)
            . takeWhile (not . budgetCategory c . snd)
            . reverse
            $ journal) $ activeCategories
    let totals = zip activeCategories spentEach
    let
        (budgets, savings)
            = partition ((==Expense) . getBudgetType)
            $ allBudgets
    let totalBudget = sum . fmap getMonthlyValue $ (budgets ++ savings)
    now <- liftIO getTime
    let startOfMonth = (\(y, m, _) -> fromGregorian y m 1) . toGregorian . localDay $ now
    let disposable = totalDisposable . filter ((<startOfMonth) . fst) $ journal
    let savedTotals = fmap (\(c, j) -> (c, totalSaved j)) . Fizz.categories $ journal
    $(whamletFile "budgets.hamlet")
    toWidget $(cassiusFile "budgets.cassius")
    addScriptRemote "http://code.jquery.com/jquery-1.10.2.min.js"
    toWidget $(juliusFile "budgets.julius")

getBudgetR :: CategoryPiece -> Handler Html
getBudgetR _ = defaultLayout [whamlet|Content!|]

putBudgetR :: CategoryPiece -> Handler ()
putBudgetR c = do
    mv <- lookupPostParam "value"
    mf <- lookupPostParam "freq"
    mt <- lookupPostParam "type"
    let
        mbe = newBudgetEntry (unwrap c)
            <$> (mv >>= maybeReadT)
            <*> (mf >>= maybeReadT)
            <*> (mt >>= maybeReadT)
    case mbe of
        Just be -> liftIO $ Fizz.budget be
        Nothing -> return ()

deleteBudgetR :: CategoryPiece -> Handler ()
deleteBudgetR cp = liftIO $ do
    journal <- Fizz.loadJournal
    let budget = mostRecentBudgets journal
    case lookup (unwrap cp) budget of
        (Just be) -> Fizz.budget be
        Nothing -> return ()

deleteSavingsR :: CategoryPiece -> Handler ()
deleteSavingsR cp = liftIO $ do
    journal <- Fizz.loadJournal
    let savingsToRealize = getSavings journal
    Fizz.realize (newExpenseEntry c savingsToRealize ("Realized " ++ printCategory c))
    where
        c = unwrap cp
        getSavings = totalSaved
            . filter ((==c) . getCategory . snd)
            . takeWhile
                ( not
                . (\e -> isRealize e && getCategory e == c)
                . snd)
            . reverse

getExpensesR :: Handler Html
getExpensesR = defaultLayout $ do
    journal <- liftIO Fizz.loadJournal
    let rows = toRows . groupExpenses $ journal
    toWidget $(cassiusFile "budgets.cassius")
    $(whamletFile "expenses.hamlet")
    where
        groupExpenses
            = M.toAscList
            . M.fromListWith (++)
            . fmap (\e -> (getExpenseCategory e, [e]))
            . catMaybes
            . fmap extractEntry
            . fmap snd
        toRow :: (Category, [ExpenseEntry]) -> (Category, Double, Int, Double)
        toRow (c, es) =
            ( c
            , sum . fmap getExpenseValue $ es
            , length es
            , meanSpent es
            )
        toRows :: [(Category, [ExpenseEntry])]
            -> [(Category, Double, Int, Double)]
        toRows = sortBy (\(_, t1, _, _) (_, t2, _, _) -> compare t2 t1)
            . filter (\(_, _, n, _) -> n > 0)
            . fmap toRow
        meanSpent :: [ExpenseEntry] -> Double
        meanSpent [] = 0
        meanSpent es = (/ (fromIntegral . length $ es))
            . sum
            . fmap getExpenseValue
            $ es

extractEntry :: Entry -> Maybe ExpenseEntry
extractEntry (Spend e) = Just e
extractEntry (Save e) = Just e
extractEntry _ = Nothing

getExpenseCategoryR :: CategoryPiece -> Handler Html
getExpenseCategoryR cat = defaultLayout $ do
    journal <- liftIO Fizz.loadJournal
    let expenses = catMaybes . maybe [] (fmap (\(t, e) -> maybe Nothing (Just . (,) t) (extractEntry e))) . lookup (unwrap cat) . categories $ journal
    let rows = toRows expenses
    toWidget $(cassiusFile "budgets.cassius")
    $(whamletFile "expense.hamlet")
    where
        toRow :: (Timestamped ExpenseEntry) -> (Double, String, String)
        toRow e = (getExpenseValue . snd $ e
            , getExpenseDescription . snd $ e
            , show . getTimestamp $ e)
        toRows :: [Timestamped ExpenseEntry] -> [(Double, String, String)]
        toRows = fmap toRow
            . reverse
            . sortBy (compare `on` getTimestamp)

postExpenseCategoryR :: CategoryPiece -> Handler ()
postExpenseCategoryR _ = undefined

getDashR :: Handler Html
getDashR = defaultLayout $ do
    now <- liftIO getTime
    journal <- liftIO Fizz.loadJournal
    let rows = getDashRows (localDay now) journal
    toWidget $(cassiusFile "budgets.cassius")
    $(whamletFile "dash.hamlet")

getDashRows :: Day -> Journal -> [(String, String, String, Double, Double, Double, Double)]
getDashRows today journal = fmap toRow expenses
    where
        (monthStart, monthEnd) = (toMonthStart today, toMonthEnd today)
        thisMonth = filter (between monthStart monthEnd . getTimestamp) journal
        lastMonth = filter ((<monthStart) . getTimestamp) journal
        lastMonthBalances = M.map (\j -> totalBudgeted j - totalSpent j) . M.fromList . categories $ lastMonth
        budgets = M.fromList . mostRecentBudgets $ journal
        expenses = filter (isSpend . snd) thisMonth
        toRow (day, Spend e) = (show day,
            printCategory . getExpenseCategory $ e,
            getExpenseDescription e,
            getExpenseValue e,
            maybe 0 getBudgetValue . M.lookup (getExpenseCategory e) $ budgets,
            (maybe 0 getBudgetValue . M.lookup (getExpenseCategory e) $ budgets) - getExpenseValue e,
            maybe 0 id . M.lookup (getExpenseCategory e) $ lastMonthBalances)
        toRow _ = error "Non-Spend Entry in toRow"

toMonthStart :: Day -> Day
toMonthStart = (\(y, m, _) -> fromGregorian y m 1)
    . toGregorian

toMonthEnd :: Day -> Day
toMonthEnd = (\(y, m, _) -> fromGregorian y (m + 1) 1)
    . toGregorian

atMidnight :: Day -> LocalTime
atMidnight = flip LocalTime (TimeOfDay 0 0 0)

maybeReadT :: Read a => T.Text -> Maybe a
maybeReadT s =
    case reads . T.unpack $ s of
        ((v,_):_) -> Just v
        _ -> Nothing

maybeRead :: Read a => String -> Maybe a
maybeRead s =
    case reads s of
        ((v,_):_) -> Just v
        _ -> Nothing

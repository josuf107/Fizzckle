{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Fizz
import Fizz.Core as Fizz
import Fizz.Store as Fizz
import Fizz.Utils (showDollars)

import Control.Applicative
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
    setTitle "Fizckle"
    journal <- liftIO loadJournal
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
    let totalIncome = totalEarned journal
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
        Just be -> liftIO $ budget be
        Nothing -> return ()

deleteBudgetR :: CategoryPiece -> Handler ()
deleteBudgetR cp = liftIO $ do
    journal <- loadJournal
    let budget = mostRecentBudgets journal
    case lookup (unwrap cp) budget of
        (Just be) -> Fizz.budget be
        Nothing -> return ()

deleteSavingsR :: CategoryPiece -> Handler ()
deleteSavingsR cp = liftIO $ do
    journal <- loadJournal
    let savingsToRealize = getSavings journal
    realize (newExpenseEntry c savingsToRealize ("Realized " ++ printCategory c))
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
    journal <- liftIO loadJournal
    let rows = toRows . groupExpenses $ journal
    toWidget $(cassiusFile "budgets.cassius")
    $(whamletFile "expenses.hamlet")
    where
        groupExpenses
            = M.toAscList
            . M.fromListWith (++)
            . fmap (\(Spend e) -> (getExpenseCategory e, [e]))
            . filter isSpend
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

getExpenseCategoryR :: CategoryPiece -> Handler Html
getExpenseCategoryR cat = defaultLayout $ do
    journal <- liftIO loadJournal
    let expenses = fmap (\(t, Spend e) -> (t, e)) . filter (\e -> getCategory (snd e) == unwrap cat && isSpend (snd e)) $ journal
    let rows = toRows expenses
    toWidget $(cassiusFile "budgets.cassius")
    $(whamletFile "expense.hamlet")
    where
        toRow :: (Timestamped ExpenseEntry) -> (Double, String, String)
        toRow e = (getExpenseValue . snd $ e
            , getExpenseDescription . snd $ e
            , show . localDay . getTimestamp $ e)
        toRows :: [Timestamped ExpenseEntry] -> [(Double, String, String)]
        toRows = fmap toRow
            . reverse
            . sortBy (compare `on` getTimestamp)

postExpenseCategoryR :: CategoryPiece -> Handler ()
postExpenseCategoryR _ = undefined

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

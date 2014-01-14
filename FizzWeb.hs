{-# LANGUAGE OverloadedStrings #-}
{-# languAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Fizz
import Fizz.Core
import Fizz.Store

import Control.Applicative
import Control.Monad
import Data.Char (toLower)
import Data.List
import Data.Maybe
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
/budgets/#Category BudgetR GET PUT
/expenses ExpensesR GET
/expenses/#Category ExpenseCategoryR GET POST
/dash/fizz FizzR POST
|]

instance PathPiece Category where
    toPathPiece = T.pack . fmap toLower . pretty
    fromPathPiece = Just . mkCategory . T.unpack

instance Yesod Fizz where
    approot = ApprootMaster (\(Fizz f) -> f)

postFizzR :: Handler RepXml
postFizzR = do
    mBody <- lookupPostParam "Body"
    let body = fromMaybe "" mBody
    $(logDebug) $ "Request: " `T.append` body
    let action = parseFizz . T.unpack $ body
    content <- liftIO . doFizz $ action
    let responseBody = wrap . T.pack $ content
    $(logDebug) $ "Response: " `T.append` responseBody
    return . RepXml . toContent $ responseBody
    where
        wrap :: T.Text -> T.Text
        wrap c = "<Response><Sms>"
            `T.append` c `T.append` "</Sms></Response>"

getBudgetsR :: Handler Html
getBudgetsR = defaultLayout $ do
    setTitle "Fizckle"
    allBudgets <- liftIO $ filter ((/=0) . getBudgetValue) <$> fmap snd <$> readBudget
    let categories = fmap getBudgetCategory allBudgets
    spentEach <- liftIO . sequence
        . fmap (\c -> sum . fmap getExpenseValue <$> readCurrentExpenses c)
        $ categories
    let totals = zip categories spentEach
    let (budgets, incomes) = partition ((>0) . getBudgetValue) allBudgets
    let totalBudget = sum . fmap getMonthlyValue $ budgets
    let totalIncome = sum . fmap getMonthlyValue $ incomes
    $(whamletFile "budgets.hamlet")
    toWidget $(cassiusFile "budgets.cassius")
    addScriptRemote "http://code.jquery.com/jquery-1.10.2.min.js"
    toWidget $(juliusFile "budgets.julius")

getBudgetR :: Category -> Handler Html
getBudgetR _ = defaultLayout [whamlet|Content!|]

putBudgetR :: Category -> Handler ()
putBudgetR c = do
    mv <- lookupPostParam "value"
    mf <- lookupPostParam "freq"
    let mbe = newBudgetEntry c <$> (mv >>= maybeReadT) <*> (mf >>= maybeReadT)
    case mbe of
        Just be -> liftIO $ void (writeBudgetEntry be)
        Nothing -> return ()

getExpensesR :: Handler Html
getExpensesR = defaultLayout $ do
    cats <- liftIO readCategories
    expenses <- liftIO $ mapM readAllExpenses cats
    let catMap = zip cats expenses
    let rows = toRows catMap
    toWidget $(cassiusFile "budgets.cassius")
    $(whamletFile "expenses.hamlet")
    where
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

getExpenseCategoryR :: Category -> Handler Html
getExpenseCategoryR cat = defaultLayout $ do
    expenses <- liftIO $ readAllExpenses cat
    let rows = toRows expenses
    toWidget $(cassiusFile "budgets.cassius")
    $(whamletFile "expense.hamlet")
    where
        toRow :: ExpenseEntry -> (Double, String, String)
        toRow e = (getExpenseValue e
            , getExpenseDescription e
            , show . localDay . getExpenseTime $ e)
        toRows :: [ExpenseEntry] -> [(Double, String, String)]
        toRows = fmap toRow
            . sortBy (\e1 e2 -> compare (getExpenseTime e2) (getExpenseTime e1))

postExpenseCategoryR :: Category -> Handler ()
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

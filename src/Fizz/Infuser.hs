module Fizz.Infuser where

import Control.Concurrent
import Control.Monad
import Data.Time
import qualified Data.Set as Set
import qualified Data.Map as Map

import Fizz.Core
import Fizz.Log
import Fizz.Store

start :: IO ()
start = do
    fizzLog "Infusing spending accounts"
    mapM_ infuse (Set.toList spendingAccounts)
    threadDelay (60 * 60 * 1000 * 1000)
    start

infuse :: Category -> IO ()
infuse category = do
    infusionDue <- checkInfusionDue category
    when infusionDue (earn (Nothing, getInfusion category))

checkInfusionDue :: Category -> IO Bool
checkInfusionDue category = do
    lastInfusion <- findEntry (earnCategory category)
    case lastInfusion of
        Nothing -> return True -- never infused before; go for it!
        (Just (day, _)) -> do
            today <- utctDay <$> getCurrentTime
            return $ diffDays day today > 6

getInfusion :: Category -> ExpenseEntry
getInfusion category = newExpenseEntry category (infusions Map.! category) "Weekly infusion"

infusions :: Map.Map Category Double
infusions = Map.fromList . fmap (\(category, amount) -> (mkCategory category, amount)) $
    [ ("joseph", 46.51)
    , ("food", 453.37)
    , ("susan", 157.79)
    , ("misc", 157.79)
    ]

-- 200 less a month is 46.51 less a week. I'll split that into 35
-- dollars from food and 5 dollars each from susan/misc

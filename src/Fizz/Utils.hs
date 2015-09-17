module Fizz.Utils where

import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Fixed
import Text.Printf

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

getTime :: IO LocalTime
getTime = do
    zt <- getZonedTime
    return $ zonedTimeToLocalTime zt

makeTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
makeTime year month day hour minute second
    = LocalTime
    (fromGregorian year month day)
    (TimeOfDay hour minute second)

whiteChars :: String
whiteChars = " \n\t"

fromIntegerToDouble :: Integer -> Double
fromIntegerToDouble = fromInteger

maybeRead :: Read a => String -> Maybe a
maybeRead s =
    case reads s of
        ((r,_):_) -> Just r
        _ -> Nothing

ifxy :: Bool -> a -> a -> a
ifxy True x _ = x
ifxy _ _ y = y

between :: Ord a => a -> a -> a -> Bool
between begin end x = x >= begin && x =< end

getDom :: LocalTime -> Int
getDom = getMonthDateDom . getMonthDate

getDow :: LocalTime -> Int
getDow = getWeekDateDow . getWeekDate

getMonth :: LocalTime -> Int
getMonth = getMonthDateMonth . getMonthDate

getWeek :: LocalTime -> Int
getWeek = getWeekDateWeek . getWeekDate

getYear :: LocalTime -> Integer
getYear = getWeekDateYear . getWeekDate

getMonthDate :: LocalTime -> MonthDate
getMonthDate t =
    let
        (_, month, dom) = toGregorian . localDay $ t
    in
        MonthDate month dom

getWeekDate :: LocalTime -> WeekDate
getWeekDate t =
    let
        (year, week, dow) = toWeekDate . localDay $ t
    in
        WeekDate year week dow

getDiffTime :: LocalTime -> DiffTime
getDiffTime = timeOfDayToTime
    . localTimeOfDay

isEmpty :: String -> Bool
isEmpty = (=="") . filter (not . flip elem whiteChars)

showDollars :: Double -> String
showDollars d
    | d >= 0 = printf "$%.2f" d
    | otherwise = printf "-$%.2f" (abs d)

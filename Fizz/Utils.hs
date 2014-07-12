module Fizz.Utils where

import Data.Time

getTime :: IO LocalTime
getTime = do
    zt <- getZonedTime
    return $ zonedTimeToLocalTime zt

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

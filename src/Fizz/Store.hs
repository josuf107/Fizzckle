module Fizz.Store
    ( record
    , budget
    , spend
    , save
    , earn
    , realize
    , redo
    , queryBack
    , queryRange
    , queryUntil
    , findEntry
    , loadJournal
    )
where

import Fizz.Core
import Fizz.Utils

import Data.List
import Data.Maybe
import Data.Time

timestamp :: Entry -> IO (Timestamped Entry)
timestamp e = getCurrentTime >>= (\t -> return (utctDay t, e))

budget :: BudgetEntry -> IO ()
budget = record . Budget

spend :: ExpenseEntry -> IO ()
spend = record . Spend

save :: ExpenseEntry -> IO ()
save = record . Save

earn :: ExpenseEntry -> IO ()
earn = record . Earn

realize :: ExpenseEntry -> IO ()
realize = record . Realize

redo :: Entry -> IO ()
redo = record . Redo

record :: Entry -> IO ()
record e = timestamp e >>= strictAppend journal . (++"\n") . show

queryBack :: Integer -> IO Journal
queryBack lookback = do
    now <- getTime
    queryRange (addDays (negate lookback) (localDay now)) (localDay now)

queryRange :: Day -> Day -> IO Journal
queryRange start end
    = filter (between start end . getTimestamp)
    <$> loadJournal

queryUntil :: (Entry -> Bool) -> IO Journal
queryUntil test
    = takeWhile (not . test . snd)
    . reverse
    <$> loadJournal

findEntry :: (Entry -> Bool) -> IO (Maybe (Timestamped Entry))
findEntry test = find (test . snd) . reverse <$> loadJournal

loadJournal :: IO Journal
loadJournal
    = catMaybes
    . fmap maybeRead
    . lines
    <$> strictRead journal

journal :: FilePath
journal = "data/journal"

strictAppend :: FilePath -> String -> IO ()
strictAppend fn s = s `seq` appendFile fn s

strictRead :: FilePath -> IO String
strictRead fn = readFile fn >>= \t -> seq t (return t)

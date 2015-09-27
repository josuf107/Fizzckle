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
import Fizz.Log

import Data.Either
import Data.List
import Data.Time

ensureTimestamp :: MaybeTimestamped Entry -> IO (Timestamped Entry)
ensureTimestamp (Nothing, e) = getCurrentTime >>= (\t -> return (utctDay t, e))
ensureTimestamp (Just t, e) = return (t, e)

budget :: MaybeTimestamped BudgetEntry -> IO ()
budget = record . withSecond Budget

spend :: MaybeTimestamped ExpenseEntry -> IO ()
spend = record . withSecond Spend

save :: MaybeTimestamped ExpenseEntry -> IO ()
save = record . withSecond Save

earn :: MaybeTimestamped ExpenseEntry -> IO ()
earn = record . withSecond Earn

realize :: MaybeTimestamped ExpenseEntry -> IO ()
realize = record . withSecond Realize

redo :: MaybeTimestamped Entry -> IO ()
redo = record . withSecond Redo

withSecond :: (b -> c) -> (a, b) -> (a, c)
withSecond f (a, b) = (a, f b)

record :: MaybeTimestamped Entry -> IO ()
record e = ensureTimestamp e >>= strictAppend journal . (++"\n") . show

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
loadJournal = do
    (badEntries, goodEntries) <- partitionEithers
        . fmap tryRead
        . lines
        <$> strictRead journal
    mapM_ fizzLog badEntries
    return goodEntries

tryRead :: Read a => String -> Either String a
tryRead s = maybe (Left $ "Couldn't parse: " ++ s) Right (maybeRead s)

journal :: FilePath
journal = "data/journal"

strictAppend :: FilePath -> String -> IO ()
strictAppend fn s = s `seq` appendFile fn s

strictRead :: FilePath -> IO String
strictRead fn = readFile fn >>= \t -> seq t (return t)

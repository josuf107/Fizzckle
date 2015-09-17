module Fizz.Log where

import Data.Time

fizzLog :: String -> IO ()
fizzLog s = do
    getCurrentTime >>= (putStr . (++": ") . show)
    putStrLn s

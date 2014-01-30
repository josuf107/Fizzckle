{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Conduit
import System.Environment

root :: String
root = "localhost:3000"

defaultMessage :: String
defaultMessage = "ping"

main :: IO ()
main = do
    args <- getArgs
    let m = if null args then defaultMessage else unwords args
    initReq <- parseUrl $ "http://" ++ root ++ "/dash/fizz"
    let request = urlEncodedBody [("Body", BS.pack m), ("From", "+18001234567")] initReq
    BS.putStrLn $ queryString request
    r <- withManager $ httpLbs request 
    LBS.putStrLn $ responseBody r

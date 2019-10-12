module Main where

import Lib

import Control.Concurrent
import System.IO
import Control.Exception
import Data.Text
import Data.Text.IO
import Data.List

interval = 7*10*1000*1000 {- 70 second -}

main :: IO()
main = do
 hSetEcho stdin False
 botconf <- getAPIkeys ["API key :", "API secret key :", "Access token :", "Access token secret :"]
 hSetEcho stdin True
 runBot botconf (pack "") >> System.IO.putStrLn "fin"
 
getAPIkeys :: [String] -> IO [String]
getAPIkeys [] = return []
getAPIkeys (m:messages) = do
 System.IO.putStr m 
 hFlush stdout
 api <- System.IO.getLine 
 putChar '\n'
 getAPIkeys messages >>= (\res -> return (api:res))

runBot :: [String] -> Text -> IO()
runBot botconf twid = do
 timeline <- getTL twid botconf
 case timeline of
  Left e   -> System.IO.putStrLn "error :: runBot getTL error"
  Right tl -> do
    favReplyPointLeft botconf ((Data.List.filter idDiff) tl) 
    threadDelay interval
    runBot botconf (if Data.List.null tl then twid else (id_str.Data.List.head) tl)
 where
  idDiff :: Tweet -> Bool
  idDiff tweet = if Data.Text.null twid then if pointleftbot /= (unpack.screen_name.user) tweet then True else False else
                    ((read.unpack) twid :: Int) < ((read.unpack.id_str) tweet :: Int) && pointleftbot /= (unpack.screen_name.user) tweet

favReplyPointLeft :: [String] -> [Tweet] -> IO()
favReplyPointLeft botconf []      = return ()
favReplyPointLeft botconf (tw:tl) = 
 case elem pointleft ((unpack.text) tw) of 
  False -> favReplyPointLeft botconf tl
  True  -> postLike (id_str tw) botconf >> 
           replyPointLeft (id_str tw) ((screen_name.user) tw) botconf >> 
           favReplyPointLeft botconf tl
 

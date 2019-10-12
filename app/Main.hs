module Main where

import Lib

import Control.Concurrent
import System.IO
import Control.Exception
import Data.Text
import Data.Text.IO
import Data.List

getint  = 7*10*1000*1000 {- 70 second -}
postint = 4*10*1000*1000 {- 40 second -}

main :: IO()
main = do
 hSetEcho stdin False
 botconf <- getAPIkeys ["API key :", "API secret key :", "Access token :", "Access token secret :"]
 hSetEcho stdin True
 tweet <- newMVar [] :: IO (MVar [Tweet])
 forkIO $ favReplyPointLeft botconf tweet
 runBot botconf (pack "") tweet >> System.IO.putStrLn "fin"
 
getAPIkeys :: [String] -> IO [String]
getAPIkeys [] = return []
getAPIkeys (m:messages) = do
 System.IO.putStr m 
 hFlush stdout
 api <- System.IO.getLine 
 putChar '\n'
 getAPIkeys messages >>= (\res -> return (api:res))

favReplyPointLeft :: [String] -> MVar [Tweet] -> IO()
favReplyPointLeft botconf tweet = do
 tl <- takeMVar tweet
 if Data.List.null tl then putMVar tweet tl >> favReplyPointLeft botconf tweet 
 else do
  postLike ((id_str.Data.List.head) tl) botconf 
  replyPointLeft ((id_str.Data.List.head) tl) ((screen_name.user.Data.List.head) tl) botconf
  putMVar tweet tl
  threadDelay postint
  favReplyPointLeft botconf tweet

runBot :: [String] -> Text -> MVar [Tweet]-> IO()
runBot botconf twid tweet = do
 timeline <- getTL twid botconf
 case timeline of
  Left e   -> System.IO.putStrLn "error :: runBot getTL error"
  Right tl -> do
    takeMVar tweet >>= (\beftweet -> putMVar tweet (beftweet ++ ((Data.List.filter idDiff) tl) ))
    threadDelay getint
    runBot botconf (if Data.List.null tl then twid else (id_str.Data.List.head) tl) tweet
 where
  idDiff :: Tweet -> Bool
  idDiff tweet = if Data.Text.null twid then if pointleftbot /= (unpack.screen_name.user) tweet then True else False else
                    ((read.unpack) twid :: Int) < ((read.unpack.id_str) tweet :: Int) && 
                    pointleftbot /= (unpack.screen_name.user) tweet &&
                    elem pointleft ((unpack.text) tweet)


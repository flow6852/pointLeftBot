{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Lib ( User (..)
           , Tweet (..)
           , PostTL (..)
           , pointleft
           , pointleftbot
           , getTL
           , replyPointLeft
           , postLike
           , getAPIkeys) where

import Control.Concurrent
import Data.Text
import Data.Text.IO 
import Data.Text.Encoding
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Web.Authenticate.OAuth
import Data.ByteString.Lazy.Internal
import Control.Monad.IO.Class
import System.IO

--get TL parser

data User = User { screen_name :: Text} deriving (Show)
$(deriveJSON defaultOptions  ''User)

data Tweet = Tweet { text     :: Text
                   , id_str   :: Text 
                   , user     :: User } deriving (Show)
$(deriveJSON defaultOptions  ''Tweet)

data GetTimeLine = GetTimeLine { tlcount :: Text
                               , tlsince_id :: Int } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 2 } ''GetTimeLine)

--post TL parser
data PostTL = PostTL { post_id_str :: Text} deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 5 } ''PostTL)

countsize = "5"
pointleft = '👈'
pointleftbot = "pointLeftBot"

postLike :: Text -> [String] -> IO()
postLike twid botconf = do
 req         <- parseRequest $ "https://api.twitter.com/1.1/favorites/create.json"
 manager     <- newManager tlsManagerSettings
 let postReq = urlEncodedBody [("id", encodeUtf8 twid)] req
 httpManager postReq botconf
 return ()

replyPointLeft :: Text -> Text -> [String] -> IO()
replyPointLeft twid user botconf = do
 req         <- parseRequest $ "https://api.twitter.com/1.1/statuses/update.json?in_reply_to_status_id=" ++ unpack twid
 manager     <- newManager tlsManagerSettings
 let message = "@" ++ (unpack user) ++ "\n" ++ [pointleft]
 let postReq = urlEncodedBody [("status", (encodeUtf8.pack) message)] req
 httpManager postReq botconf
 return ()

getTL :: Text -> [String] -> IO (Either String [Tweet])
getTL twid botconf = do
 response <- do
  req <- parseRequest $ "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ countsize 
                      ++ (if Data.Text.null twid then "" else "&since_id="++unpack twid)
  httpManager req botconf
 return $ eitherDecode $ responseBody response

httpManager :: Request -> [String] -> IO(Response Data.ByteString.Lazy.Internal.ByteString)
httpManager req botconf = do
 (myOAuth, myCredential) <- botuser botconf
 signedReq <- signOAuth myOAuth myCredential req
 manager <- newManager tlsManagerSettings
 Network.HTTP.Conduit.httpLbs signedReq manager

botuser :: [String] -> IO(OAuth,Credential)
botuser botsparameter = do
 let  myOAuth      = newOAuth { oauthServerName     = "api.twitter.com"
                              , oauthConsumerKey    = C.pack(Prelude.head botsparameter)
                              , oauthConsumerSecret = C.pack(botsparameter !! 1)
  }
      myCredential = newCredential (C.pack(botsparameter !! 2)) (C.pack(botsparameter !! 3))
 return (myOAuth, myCredential)
 
getAPIkeys :: [String] -> IO [String]
getAPIkeys [] = return []
getAPIkeys (m:messages) = do
 Prelude.putStr m 
 hFlush stdout
 api <- Prelude.getLine 
 Prelude.putChar '\n'
 getAPIkeys messages >>= (\res -> return (api:res))


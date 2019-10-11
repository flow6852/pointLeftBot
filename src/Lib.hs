{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module TwitterAPI ( GetDM (..)
                  , GetMessageData (..)
                  , GetMessageCreate (..)
                  , GetEvents (..)
                  , PostRecipient (..)
                  , PostDM (..)
                  , PostMessageData (..)
                  , PostMessageCreate (..)
                  , PostEvent (..)
                  , Tweet (..)
                  , PostTL (..)
                  , User (..)
                  , getTL
                  , getUser
                  , tweet) where

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

-- get DM parser
data GetDM = GetDM { gettext :: Text
                   } deriving(Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''GetDM)

data GetMessageData = GetMessageData { getmessage_data :: GetDM
                                     , getsender_id :: Text
                                      } deriving(Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''GetMessageData)

data GetMessageCreate = GetMessageCreate { getmessage_create :: GetMessageData
                                         , getcreated_timestamp :: Text
                                          } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''GetMessageCreate)

data GetEvents = GetEvents { getevents :: [GetMessageCreate]
                           } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''GetEvents)

-- post DM parser
data PostRecipient = PostRecipient { postrecipient_id :: Text
                                   } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostRecipient)

data PostDM = PostDM { posttext :: Text
                     } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostDM)

data PostMessageData = PostMessageData { postmessage_data :: PostDM 
                                       , posttarget :: PostRecipient
                                        } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostMessageData)

data PostMessageCreate = PostMessageCreate { posttype :: Text
                                           , postmessage_create :: PostMessageData
                                           } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostMessageCreate)

data PostEvent = PostEvent { postevent :: PostMessageCreate
                              } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 4 } ''PostEvent)

--get TL parser
data Tweet = Tweet { text :: Text
                   , entities :: TweetEntities } deriving (Show)
$(deriveJSON defaultOptions  ''Tweet)

data TweetEntities = TweetEntities { urls :: TweetUrls } deriving (Show)
$(deriveJSON defaultOptions  ''TweetEntities)

data TweetUrls = TweetUrls { expanded_url :: Text } deriving (Show)
$(deriveJSON defaultOptions  ''TweetUrls)

--post TL parser
data PostTL = PostTL { id_str :: Text} deriving (Show)
$(deriveJSON defaultOptions ''PostTL)

--get User parser
data User = User { gid_str :: Text } deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 1 }  ''User)

postLike :: Text -> IO()
postLike twid = do
 req         <- parseRequest $ "https://api.twitter.com/1.1/favorites/create.json"
 manager     <- newManager tlsManagerSettings
 let postReq = urlEncodeBody [("id", encodeUtf8 twid)] req
 httpManager postReq
 return ()

replayPointLeft :: Text -> IO()
replayPointLeft url = do
 req         <- parseRequest $ url
 manager     <- newManager tlsManagerSettings
 let postReq = urlEncodeBody [("id", encodeUtf8 "👈")] req
 httpManager postReq
 return ()

getTL :: IO (Either String [Tweet])
getTL = do
 response <- do
  req <- parseRequest  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=1"
  httpManager req
 return $ eitherDecode $ responseBody response

httpManager :: Request -> IO(Response Data.ByteString.Lazy.Internal.ByteString)
httpManager req = do
 (myOAuth, myCredential) <- botuser
 signedReq <- signOAuth myOAuth myCredential req
 manager <- newManager tlsManagerSettings
 Network.HTTP.Conduit.httpLbs signedReq manager

{- have to change -}
botuser :: IO(OAuth,Credential)
botuser = do
 botsparameter <- Prelude.lines <$> Prelude.readFile twitterbotconf
 let  myOAuth      = newOAuth { oauthServerName     = "api.twitter.com"
                              , oauthConsumerKey    = C.pack(Prelude.head botsparameter)
                              , oauthConsumerSecret = C.pack(botsparameter !! 1)
  }
      myCredential = newCredential (C.pack(botsparameter !! 2)) (C.pack(botsparameter !! 3))
 return (myOAuth, myCredential)


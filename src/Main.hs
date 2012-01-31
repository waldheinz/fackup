
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Debug.Trace

import Control.Applicative ((<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe, Value)
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.ByteString.Lazy (ByteString, toChunks)
import qualified Data.ByteString as BS (ByteString, concat)
import Data.Functor ((<$>))
import Data.Maybe
import qualified Data.Vector as V
import Network.OAuth.Consumer
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response

reqUrl   = fromJust $ parseURL "http://www.flickr.com/services/oauth/request_token"
accUrl   = fromJust $ parseURL "http://www.flickr.com/services/oauth/access_token"
srvUrl   = fromJust $ parseURL "http://api.flickr.com/services/rest?nojsoncallback=1&format=json"
app      = Application "eeb2b5a4cde6988dec44195952ce1cbe" "fdc50914cdbc3bd8" OOB
token    = fromApplication app

response = runOAuthM token $ do
   signRq2 HMACSHA1 Nothing reqUrl >>= oauthRequest CurlClient
   cliAskAuthorization $ \t -> "http://www.flickr.com/services/oauth/authorize?oauth_token=" ++ 
                       (head . find (=="oauth_token") . oauthParams) t
   signRq2 HMACSHA1 Nothing accUrl >>= oauthRequest CurlClient
   t <- getToken
   let
        secret = head . find (== "oauth_token_secret") . oauthParams
        token = head . find (== "oauth_token") . oauthParams
   return (token t, secret t)

storedAuth = ("72157628981807463-1c78795ce3b82eb0","5b71c229f5d640b5")

authToken :: (String, String) -> Token
authToken (t, s) = AccessToken app fs where
   fs = insert ("oauth_token_secret", s) $ singleton ("oauth_token", t)

callFlickr :: Token -> String -> IO Response
callFlickr t m = runOAuthM t $ signRq2 HMACSHA1 Nothing url >>= serviceRequest CurlClient where
   url =  srvUrl { qString = insert ("method", m) $ qString srvUrl }

---------------------------------------------------------------------------
-- Photo Sets
---------------------------------------------------------------------------

data PhotoSet = PhotoSet 
   { psId     :: String
   , name     :: String
   } deriving (Show)

instance FromJSON PhotoSet where
   parseJSON (Object v) = PhotoSet <$>
                             v .: "id" <*>
                             (v .: "title" >>= \x -> x .: "_content")
   parseJSON _ = mzero
   
unlazy :: Data.ByteString.Lazy.ByteString -> BS.ByteString
unlazy = BS.concat . toChunks

getSets :: Token -> IO [PhotoSet]
getSets t = do
   r <- parseOnly json . unlazy . rspPayload <$> callFlickr t "flickr.photosets.getList"
   return $ fromMaybe (error "Invalid response from Flickr") $
            parseMaybe parseSets $ either error id r
   where
      parseSets :: Value -> Data.Aeson.Types.Parser [PhotoSet]
      parseSets (Object o) = do
         (Array a) <- o .: "photosets" >>= \x -> x .: "photoset"
         mapM parseJSON $ V.toList a
      parseSets _ = mzero

main :: IO ()
main = do
   let t = authToken storedAuth
   r <- getSets t
   print r
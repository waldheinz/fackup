
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Debug.Trace

import Control.Applicative ((<*>))
import Control.Monad (forM_, mzero)
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

callFlickr
   :: Token       -- ^ auth token
   -> String      -- ^ method name
   -> FieldList   -- ^ additional parameters
   -> (Value -> Parser a)
   -> IO a
callFlickr t m ps p = runOAuthM t $ do
   req <- signRq2 HMACSHA1 Nothing $
          srvUrl { qString = union ps $ insert ("method", m) $ qString srvUrl }
   res <- parseOnly json . unlazy . rspPayload <$> serviceRequest CurlClient req
   return $ fromMaybe (error "cannot parse Flickr response") $
            parseMaybe p $ either error id res
   where
      unlazy = BS.concat . toChunks

--------------------------------------------------------------------------------
-- Photo Sets
--------------------------------------------------------------------------------

data PhotoSet = PhotoSet 
   { psId     :: String
   , psName   :: String
   } deriving (Show)

instance FromJSON PhotoSet where
   parseJSON (Object v) = PhotoSet <$>
                             v .: "id" <*>
                             (v .: "title" >>= \x -> x .: "_content")
   parseJSON _ = mzero

getSets :: Token -> IO [PhotoSet]
getSets t = callFlickr t "flickr.photosets.getList" empty parseSets where
   parseSets :: Value -> Data.Aeson.Types.Parser [PhotoSet]
   parseSets (Object o) = do
      (Array a) <- o .: "photosets" >>= \x -> x .: "photoset"
      mapM parseJSON $ V.toList a
   parseSets _ = mzero

--------------------------------------------------------------------------------
-- Individual Photos
--------------------------------------------------------------------------------

data Photo = Photo
   { pId    :: String
   , title  :: String
   } deriving (Show)

-- | gets one page of photos from a set
getPhotos' :: Token -> PhotoSet -> Int -> IO [Photo]
getPhotos' t s p = callFlickr t
                  "flickr.photosets.getPhotos"
                  (insert ("page", show p) $ singleton ("photoset_id", psId s))
                  pp where
                     
   pp :: Value -> Data.Aeson.Types.Parser [Photo]
   pp = undefined

getPhotos :: Token -> PhotoSet -> IO [Photo]
getPhotos t s = undefined
   
main :: IO ()
main = do
   let t = authToken storedAuth
   putStrLn "getting photo sets..."
   sets <- getSets t
   putStrLn $ "found " ++ (show $ length sets) ++ " sets"
   
   forM_ sets $ \s -> do
      putStrLn $ "processing set \"" ++ (psName s) ++ "\" (" ++ psId s ++ ") ..."
      ps <- getPhotos t s
      putStrLn $ "   contains " ++ (show $ length ps) ++ " photos"
      
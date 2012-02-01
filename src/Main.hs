
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Debug.Trace

import Control.Applicative ((<*>))
import Control.Monad (forM_, mzero, unless)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe, Value)
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.Attoparsec.Number
import qualified Data.ByteString.Lazy as BSL (toChunks)
import qualified Data.ByteString as BS (ByteString, concat, writeFile)
import Data.Functor ((<$>))
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Curl.Download as DC
import Network.OAuth.Consumer
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import System.Directory
import System.FilePath ((</>), (<.>), makeValid)

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
      unlazy = BS.concat . BSL.toChunks

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
-- Individual Photo Infos
--------------------------------------------------------------------------------

data Photo = Photo
   { pId       :: String
   , pTitle    :: String
   , pFormat   :: String
   , pURL      :: String
   } deriving (Show)

instance FromJSON Photo where
   parseJSON (Object o) = Photo <$>
      o .: "id" <*>
      o .: "title" <*>
      o .: "originalformat" <*>
      o .: "url_o"
   parseJSON _ = mzero

-- | gets one page of photos from a set
getPhotoList
   :: Token -- ^ access token
   -> String -- ^ method name
   -> T.Text -- ^ how the list is called in the JSON reponse
   -> FieldList -- ^ additional request parameters
   -> Integer -> IO (V.Vector Photo)
getPhotoList t m jn fl p = do
   putStrLn $ "   getting page " ++ show p
   (pg, res) <- callFlickr t m
      (insert ("page", show p) $
       insert ("extras", "url_o,original_format") fl) pp

   if p >= pg
      then return res
      else do
         res' <- getPhotoList t m jn fl (p+1)
         return $ res V.++ res'
   
   where
      pp :: Value -> Data.Aeson.Types.Parser (Integer, V.Vector Photo)
      pp (Object o) = do
         ps <- o .: jn
         (Number (I pg)) <- ps .: "pages"
         a <- ps .: "photo"
         return (pg, a)
      pp _ = mzero

-- | gets all photos from the specified set
getPhotos :: Token -> PhotoSet -> IO (V.Vector Photo)
getPhotos t s = getPhotoList t "flickr.photosets.getPhotos" "photoset"
      (singleton ("photoset_id", psId s)) 1

--------------------------------------------------------------------------------
-- Photos not belonging to any set
--------------------------------------------------------------------------------

notInSet :: Token -> IO (V.Vector Photo)
notInSet t = getPhotoList t "flickr.photos.getNotInSet" "photos" empty 1

--------------------------------------------------------------------------------
-- Actual Photo Download
--------------------------------------------------------------------------------

down :: FilePath -> Photo -> IO ()
down dir p = do
   de <- doesDirectoryExist dir 
   unless de $ do
      putStrLn $ "creating dir " ++ show dir
      createDirectory dir
      
   putStrLn $ "   downloading " ++ show (pURL p)
   img <- DC.openURI $ pURL p
   
   case img of
        (Left e) -> error e
        (Right d) -> do
           BS.writeFile (dir </> fname <.> (pFormat p)) d
   where
      fname :: FilePath
      fname
         | null (pTitle p) = pId p
         | otherwise = pTitle p

main :: IO ()
main = do
   let t = authToken storedAuth

   putStrLn "getting photos not belonging to any set..."
   nis <- notInSet t

   V.mapM_ (down "# not in set") nis
   
   putStrLn "getting photo sets..."
   sets <- getSets t
   putStrLn $ "found " ++ (show $ length sets) ++ " sets"
   
   forM_ sets $ \s -> do
      putStrLn $ "processing set \"" ++ (psName s) ++ "\" (" ++ psId s ++ ") ..."
      ps <- getPhotos t s
      putStrLn $ "   contains " ++ (show $ V.length ps) ++ " photos, downloading..."
      V.mapM_ (down $ makeValid $ psName s) ps
      
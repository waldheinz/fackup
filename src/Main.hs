
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Applicative ((<*>))
import qualified Control.Exception as CE
import Control.Monad (forM_, mzero, unless)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe, Value)
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.Attoparsec.Number
import qualified Data.ByteString.Lazy as BSL (toChunks)
import qualified Data.ByteString as BS (ByteString, concat, writeFile)
import Data.Functor ((<$>))
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Curl.Download as DC
import Network.OAuth.Consumer
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import System.Directory
import System.FilePath ((</>), (<.>), makeValid, splitDirectories)

reqUrl   = fromJust $ parseURL "http://www.flickr.com/services/oauth/request_token"
accUrl   = fromJust $ parseURL "http://www.flickr.com/services/oauth/access_token"
srvUrl   = fromJust $ parseURL "http://api.flickr.com/services/rest?nojsoncallback=1&format=json"
app      = Application "eeb2b5a4cde6988dec44195952ce1cbe" "fdc50914cdbc3bd8" OOB

-- permanent credentials for accessing Flickr after a successfull authentication
type StoredAuth = (String, String)

-- tries to authenticate with Flickr
doAuth :: IO StoredAuth
doAuth = runOAuthM (fromApplication app) $ do
   signRq2 HMACSHA1 Nothing reqUrl >>= oauthRequest CurlClient
   cliAskAuthorization $ \t -> "http://www.flickr.com/services/oauth/authorize?oauth_token=" ++ 
                       (head . find (=="oauth_token") . oauthParams) t
   signRq2 HMACSHA1 Nothing accUrl >>= oauthRequest CurlClient
   t <- getToken
   let  secret = head . find (== "oauth_token_secret") . oauthParams
        token = head . find (== "oauth_token") . oauthParams
   return (token t, secret t)

-- | reads authentication info from program settings directory
loadAuth :: IO StoredAuth
loadAuth = do
   dir <- getAppUserDataDirectory "fackup"
   de <- doesDirectoryExist dir 
   unless de $ createDirectory dir
   auth <- readFile (dir </> "auth") 
   return $! read auth

saveAuth :: StoredAuth -> IO ()
saveAuth a = do
   dir <- getAppUserDataDirectory "fackup"
   writeFile (dir </> "auth") $ show a   

authToken :: (String, String) -> Token
authToken (t, s) = AccessToken app fs where
   fs = insert ("oauth_token_secret", s) $ singleton ("oauth_token", t)

-- | does an authenticated call to the Flickr API
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

-- | gets the list of photos for the currently logged in user
getSets :: Token -> IO [PhotoSet]
getSets t = callFlickr t "flickr.photosets.getList" empty parseSets where
   parseSets :: Value -> Data.Aeson.Types.Parser [PhotoSet]
   parseSets (Object o) = do
      (Array a) <- o .: "photosets" >>= \x -> x .: "photoset"
      mapM parseJSON $ V.toList a
   parseSets _ = mzero

-- | the information needed about individual photos
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

-- | gets the photos from a "photo list" (they are formatted the same,
--   so we can use a generic function for our purposes)
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

   if p >= pg -- get additional pages if needed
      then return res
      else do
         res' <- getPhotoList t m jn fl (p+1)
         return $ res V.++ res'
   
   where
      pp (Object o) = do -- parses one page of photos
         ps <- o .: jn
         (Number (I pg)) <- ps .: "pages"
         a <- ps .: "photo"
         return (pg, a)
      pp _ = mzero

-- | gets all photos from the specified set
getPhotos :: Token -> PhotoSet -> IO (V.Vector Photo)
getPhotos t s = getPhotoList t "flickr.photosets.getPhotos" "photoset"
      (singleton ("photoset_id", psId s)) 1

-- | gets the photos not belonging to any set
notInSet :: Token -> IO (V.Vector Photo)
notInSet t = getPhotoList t "flickr.photos.getNotInSet" "photos" empty 1

-- | downloads a photo and stores the file in the specified path
down'
   :: FilePath -- ^ path where to store the photo
   -> Photo -- ^ the photo to download
   -> IO ()
down' dir p = do
   de <- doesDirectoryExist dir 
   unless de $ do
      putStrLn $ "creating dir " ++ show dir
      createDirectory dir
      
   fullname <- fname
   putStrLn $ "   downloading " ++ show (pURL p) ++ " -> " ++ show fullname
   img <- DC.openURI $ pURL p
   
   case img of
        (Left e) -> error e
        (Right d) -> BS.writeFile fullname d
        
   where
      -- | makes sure the file name is unique by appending a suffix
      fname :: IO FilePath
      fname = go 0 where
         cand s -- candidate name for the given sequence number
            | s == 0 = dir </> fn <.> pFormat p
            | otherwise = dir </> (fn ++ "-" ++ show s) <.> pFormat p
            
         go s = do
            e <- doesFileExist $ cand s
            if e
               then go (s+1)
               else return $ cand s

         -- | finds a "good" name for the image file
         fn
            | null (pTitle p) = makeValid $ pId p
            | otherwise = makeValid $ intercalate "_" $
                             splitDirectories $ pTitle p
               -- make sure we don't create additional directories

-- | tries multiple times to download a single photo
down :: FilePath -> Photo -> IO ()
down fp p = go 5 where
   go 0 = putStrLn $ "FAILED to get " ++ show p
   go n = CE.catch (down' fp p)
             (\ex -> do
                putStrLn $
                   "error getting photo: " ++ show (ex :: CE.SomeException)
                go (n-1) )

main :: IO ()
main = do
   -- load / request authentication token
   t <- authToken <$>
           CE.catch loadAuth
              (\ex -> do
                 putStrLn $
                   "failed loading auth info: " ++ show (ex :: CE.IOException)
                 a <- doAuth
                 saveAuth a
                 return a)

   putStrLn "getting photos not belonging to any set..."
   nis <- notInSet t
   putStrLn $ "   contains " ++ show (V.length nis) ++ " photos, downloading..."
   V.mapM_ (down "# not in set") nis
   
   putStrLn "getting photo sets..."
   sets <- getSets t
   putStrLn $ "found " ++ show (length sets) ++ " sets"
   
   forM_ sets $ \s -> do
      putStrLn $ "processing set \"" ++ psName s ++ "\" (" ++ psId s ++ ") ..."
      ps <- getPhotos t s
      putStrLn $ "   contains " ++ show (V.length ps) ++ " photos, downloading..."
      V.mapM_ (down $ makeValid $ psName s) ps
      
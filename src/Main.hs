
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main ( main ) where

import Data.Maybe
import Network.OAuth.Consumer
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response

reqUrl   = fromJust $ parseURL "http://www.flickr.com/services/oauth/request_token"
accUrl   = fromJust $ parseURL "https://api.login.yahoo.com/oauth/v2/get_token"
srvUrl   = fromJust $ parseURL "http://query.yahooapis.com/v1/yql?q=select%20%2A%20from%20social.profile%20where%20guid%3Dme"
authUrl  = head . find (=="xoauth_request_auth_url") . oauthParams
app      = Application "<consumer key>" "<consumer secret>" OOB
token    = fromApplication app

response = runOAuthM token $ do
   signRq2 PLAINTEXT Nothing reqUrl >>= oauthRequest CurlClient
 --  cliAskAuthorization authUrl
 --  signRq2 PLAINTEXT Nothing accUrl >>= oauthRequest CurlClient
 --  signRq2 HMACSHA1 (Just $ Realm "yahooapis.com") srvUrl >>= serviceRequest CurlClient
   
main :: IO ()
main = do
   r <- response
   return ()
--   print r

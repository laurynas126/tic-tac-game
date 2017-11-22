{-# LANGUAGE OverloadedStrings #-}

module HttpClient where

import Data.Conduit.Binary (sinkFile)
import Data.String.Conversions
import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as L

getHttpData :: String -> String
getHttpData _ = "d1:cli2ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli1ei2ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli2ei2ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli2ei1ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli0ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli0ei2ee2:id22:MCWPFWuHBPApBdNBKxjbud4:prevd1:cli1ei0ee2:id25:MnQxKaeMJPTGsApXOrSwiSNnX4:prevd1:cli0ei1ee2:id22:MCWPFWuHBPApBdNBKxjbud1:v1:oe1:v1:oe1:v1:oe1:v1:oe1:v1:oe1:v1:xe1:v1:oe1:v1:xe"

getHttp :: String -> IO String
getHttp url =  do
    request' <- parseRequest url
    let request
            = setRequestMethod "GET"
            $ addRequestHeader "Content-Type" "application/bencode"
            $ addRequestHeader "Accept" "application/bencode"
            $ request'
    print (getRequestHeader "Content-Type" request)  
    response <- httpLBS request
    print "DEBUG:"
    print (getResponseStatusCode response)
    print (getResponseBody response)
    print "------------------------"
    let responseCode = getResponseStatusCode response
    if responseCode /= 200
        then return ""
        else return (cs (getResponseBody response))

postHttp :: String -> String -> IO String
postHttp url postBody = do
    request' <- parseRequest url
    let request
            = setRequestMethod "POST"
            $ setRequestBodyLBS (cs postBody)
            $ (addRequestHeader "Content-Type" "application/bencode")
            $ (addRequestHeader "Accept" "application/bencode")
            $ request'
    response <- httpLBS request
    let responseCode = getResponseStatusCode response
    if responseCode /= 200
        then return ""
        else return (cs (getResponseBody response))
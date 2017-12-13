{-# LANGUAGE OverloadedStrings #-}
module HttpServer where

import Data.String.Conversions
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Network.HTTP.Types.Method
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import Data.Monoid
import Parser

runServer :: IO ()
runServer = do
    let port = 8080
    putStrLn $ "Listening on port " ++ show port
    run port app2
    
-- app req respond = respond $
--     case pathInfo req of
--         ["history"] -> history
--         x -> postResponse req
 
app2 req respond = do
    body <- requestBody req
    let response = case pathInfo req of
            ["history"] -> history
            x -> if requestMethod req == methodPost then postResponse body
                 else invalidRequest
    respond response

history = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "Here is my history:" ]

invalidRequest :: Response
invalidRequest = responseBuilder status400 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "Invalid request method: Expected: POST, actual: GET" ]

postResponse :: B.ByteString -> Response
postResponse body = do
    let (finished, responseBody) = Parser.makeMove $ cs body
    let responseBody2
            | finished = "---Game has ended---"
            | otherwise = responseBody
    responseBuilder status200 [ ("Content-Type", "application/bencode") ] $ mconcat $ map copyByteString [cs responseBody2]
    
-- postResponse :: Request -> Response
-- postResponse req = do
--         let reqMethod = requestMethod req
--         let reqStr
--                 | reqMethod == methodPost = getResponseMove (requestBody req)
--                 | reqMethod == methodGet = return ("Invalid request method: Expected: POST, actual: GET")
--                 | otherwise = return ("?")
--         -- message <- reqStr
--         responseBuilder status200 [ ("Content-Type", "application/bencode") ] $ mconcat $ map copyByteString ["message"]

getResponseMove :: B.ByteString -> B.ByteString
getResponseMove body = "d1e1e"


index x = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
    , "<p><a href='/yay'>yay</a></p>\n" ]
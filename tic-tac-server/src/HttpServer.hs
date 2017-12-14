{-# LANGUAGE OverloadedStrings #-}
module HttpServer where

import Data.List
import qualified Data.Text as T
import Data.String.Conversions
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Network.HTTP.Types.Method
import Blaze.ByteString.Builder (copyByteString)
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import Data.Monoid
import Parser


runServer :: IO ()
runServer = do
    let port = 8080
    putStrLn $ "Listening on port " ++ show port
    let str = []
    tvar <- newTVarIO str
    run port (app tvar)
    
-- app req respond = respond $
--     case pathInfo req of
--         ["history"] -> history
--         x -> postResponse req
 
app tvar req respond = do
    gameIDs <- atomically $ readTVar tvar
    body <- requestBody req
    let response = case pathInfo req of
            ["history"] -> history gameIDs
            [x] -> if requestMethod req == methodPost then
                    postResponse body
                 else invalidRequest
            _ -> invalidRequest
    let game = head (pathInfo req)
    if length (pathInfo req) == 1 && game /= "history" && game `notElem` gameIDs then do
        a <- atomically $ writeTVar tvar ((head (pathInfo req)):gameIDs)
        respond response
    else respond response

history :: [T.Text] -> Response
history gameIDs = responseBuilder status200 [ ("Content-Type", "text/html") ] $ mconcat $ map copyByteString
    [ "<h1>Game history</h1>", "<ul><li>", cs $ intercalate "<li>" $ map (++ "</li>") $ map T.unpack gameIDs, "</ul>"]

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
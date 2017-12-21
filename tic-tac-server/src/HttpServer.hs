{-# LANGUAGE OverloadedStrings #-}
module HttpServer where

import Data.List
import qualified Data.Text as T
import Data.Text.Encoding
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

app :: TVar [T.Text] -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app tvar req respond = do
    gameIDs <- atomically $ readTVar tvar
    body <- requestBody req
    let reqMethod = requestMethod req
    let response = case pathInfo req of
            ["history"] -> history gameIDs
            [_] -> if reqMethod == methodPost then
                    postResponse body
                 else invalidRequest
            _ -> invalidRequest
    let game = head (pathInfo req)
    if reqMethod == methodPost && length (pathInfo req) == 1 && game /= "history" && game `notElem` gameIDs then do
        a <- atomically $ writeTVar tvar (game:gameIDs)
        respond response
    else respond response

history :: [T.Text] -> Response
history gameIDs = responseBuilder status200 [ ("Content-Type", "text/html") ] $ mconcat $ map copyByteString
    [ "<!DOCTYPE html>\n"
    , "<html lang=\"en\">\n"
    , "<head><title>Tic Tac Game History</title></head>\n"
    , "<body>\n"
    , "<h1>Games history</h1>\n" 
    , "<ul>\n<li>"
    , cs $ intercalate "\n<li>" $ map ((++ "</li>") . T.unpack) gameIDs, "</ul>"
    , "\n</body>"]

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
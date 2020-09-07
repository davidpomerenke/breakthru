{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString, append, fromStrict, pack)
import Data.Function ((&))
import Lib (move)
import Network.HTTP.Types (status200, status400)
import Network.Wai
  ( Application,
    Request (requestMethod),
    getRequestBodyChunk,
    responseFile,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"
  run 8080 app

-- SERVER
app :: Application
app request respond = do
  body <- getRequestBodyChunk request
  respond $
    case requestMethod request of
      -- API
      "POST" ->
        ( ( case decode $ fromStrict body of
              Just board ->
                responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                  (board & move & encode)
              _ ->
                responseLBS
                  status400
                  [("Content-Type", "text/plain")]
                  ""
          )
        )
      -- MAIN PAGE
      "GET" ->
        responseFile
          status200
          [("Content-Type", "text/html")]
          "static/index.html"
          Nothing

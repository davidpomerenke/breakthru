{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString, append, fromStrict, pack)
import Data.Function ((&))
import Flow ((<|), (|>))
import Lib (Coordinate (..), Game (..), State, breakthru, move)
import Network.HTTP.Types (status200, status400)
import Network.Wai
  ( Application,
    Request (requestMethod),
    getRequestBodyChunk,
    responseFile,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)

-- | Main function. Runs the server application.
main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"
  run 8080 app

-- | The server application running the game.
-- * Presents the static files for running the games.
-- * Provides an API representing the game AI, which is called by the static files after each move of the user.
app :: Application
app request respond = do
  body <- getRequestBodyChunk request
  respond
    <| case requestMethod request of
      -- API
      "POST" ->
        case decode $ fromStrict body of
          Just state ->
            responseLBS
              status200
              [("Content-Type", "text/plain")]
              (state |> move |> encode)
          _ ->
            responseLBS
              status400
              [("Content-Type", "text/plain")]
              ""
      -- MAIN PAGE
      "GET" ->
        responseFile
          status200
          [("Content-Type", "text/html")]
          "static/index.html"
          Nothing

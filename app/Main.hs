{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString, append, fromStrict, pack)
import Data.Function ((&))
import Debug.Trace
import Flow ((<|), (|>))
import Lib (Coordinate (..), Game (..), State, aiMove, breakthru, move)
import Network.HTTP.Types (status200, status400)
import Network.Wai
  ( Application,
    Request (rawPathInfo, requestMethod),
    getRequestBodyChunk,
    responseFile,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import Web.Browser (openBrowser)

-- | Main function. Runs the server application.
main :: IO ()
main =
  let url = "http://localhost:8000/"
   in do
        putStrLn url
        openBrowser url
        run 8000 app

-- | The server application running the game.
-- * Presents the static files for running the games.
-- * Provides an API representing the game AI, which is called by the static files after each move of the user.
app :: Application
app request respond =
  let fail =
        responseLBS
          status400
          [("Content-Type", "text/plain")]
          ""
   in do
        body <- getRequestBodyChunk request

        respond
          <| case requestMethod request of
            -- API

            "POST"
              | rawPathInfo request == "/move" ->
                case decode $ fromStrict body of
                  Just (action, state) ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      (move state action |> encode)
                  _ -> fail
              | rawPathInfo request == "/ai-move" ->
                case decode $ fromStrict body of
                  Just state ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      (state |> aiMove |> encode)
                  _ -> fail
              | rawPathInfo request == "/actions" ->
                case decode $ fromStrict body of
                  Just state ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      ( let Game {action} = breakthru
                         in state |> action |> encode
                      )
                  Nothing -> fail
              | otherwise -> fail
            -- MAIN PAGE
            "GET" ->
              responseFile
                status200
                [("Content-Type", "text/html")]
                "static/index.html"
                Nothing

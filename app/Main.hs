{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ai
import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString, append, fromStrict, pack)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Debug.Trace
import Evaluate
import Flow ((<|), (|>))
import Game
import Network.HTTP.Types (status200, status400)
import Network.Wai
  ( Application,
    Request (rawPathInfo, requestMethod),
    Response (..),
    getRequestBodyChunk,
    responseFile,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import System.Environment
import System.Random (StdGen, mkStdGen)
import Web.Browser (openBrowser)

-- | Main function. Plug in `serve` (for playing in the browser) or `compete` here, depending on what mode you want the program to start in.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["evaluate"] ->
      evaluate
    _ -> serve

-- | Runs the server application.
serve :: IO ()
serve =
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
  let Game {actions, utility, result} = breakthru

      fail =
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
              | rawPathInfo request == "/init" ->
                responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                  (initial_ |> encode)
              | rawPathInfo request == "/actions" ->
                case decode $ fromStrict body of
                  Just state ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      ( state |> actions |> encode
                      )
                  Nothing -> fail
              | rawPathInfo request == "/result" ->
                case decode $ fromStrict body of
                  Just (state, action) ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      (Game.move state action |> encode)
                  _ -> fail
              | rawPathInfo request == "/utility" ->
                case decode $ fromStrict body of
                  Just state ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      ( ( case utility state of
                            Nothing -> 0
                            Just f -> let Utility u = f Gold in u
                        )
                          |> encode
                      )
                  _ -> fail
              | rawPathInfo request == "/ai/random" ->
                case decode $ fromStrict body of
                  Just state ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      ( Ai.random (mkStdGen 137) state |> fmap (result state)
                          |> join
                          |> fromMaybe state
                          |> encode
                      )
                  _ -> fail
              | rawPathInfo request == "/ai/max" ->
                case decode $ fromStrict body of
                  Just state ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      ( Ai.max (mkStdGen 137) state |> fmap (result state)
                          |> join
                          |> fromMaybe state
                          |> encode
                      )
                  _ -> fail
              | otherwise -> fail
            -- MAIN PAGE
            "GET" ->
              responseFile
                status200
                [("Content-Type", "text/html")]
                "static/index.html"
                Nothing
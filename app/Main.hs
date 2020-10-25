{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Debug.Trace
import Evaluate
import Flow ((<|), (|>))
import Game
import Network.HTTP.Types (status200, status400)
import Network.Wai
  ( Application,
    Request (rawPathInfo, requestMethod),
    getRequestBodyChunk,
    responseFile,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import System.Random (mkStdGen)
import Web.Browser (openBrowser)
import AlphaBeta
import Minimax
import Helpers

-- | Main function. Evaluates command line arguments.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["evaluation"] ->
      evaluate
    ["display"] ->
      serve "display.html"
    _ -> serve "index.html"

-- | Runs the server application.
serve :: String -> IO ()
serve file =
  let url = "http://localhost:8000/" ++ file
   in do
        putStrLn url
        openBrowser url
        run 8000 (app file)

-- | The server application running the game.
-- * Presents the static files for running the games.
-- * Provides an API representing the game AI, which is called by the static files after each move of the user.
app :: String -> Application
app file request respond =
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
              | rawPathInfo request == "/display" ->
                responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                  (displayedState |> encode)
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
                            Just (Utility u) -> u
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
                      ( random (mkStdGen 137) state |> fmap (result state)
                          |> join
                          |> fromMaybe state
                          |> encode
                      )
                  _ -> fail
              | rawPathInfo request == "/ai/minimax" ->
                case decode $ fromStrict body of
                  Just state ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      ( alphaBeta 3 (mkStdGen 136) state |> fmap (result state)
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
                ("static/" ++ file)
                Nothing

-- | Just an arbitrary state displayed when running with command line argument `display`. For debugging purposes.
displayedState :: State
displayedState =
  State
    { lastPlayer = Just Silver,
      player = Silver,
      movedPiece = Just (Coordinate {x = 5, y = 2}),
      gold = (Just (Coordinate {x = 4, y = 4}), [Coordinate {x = 3, y = 4}]),
      silver = [Coordinate {x = 5, y = 2}]
    }
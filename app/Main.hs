{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ai
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
import System.Environment ( getArgs )
import System.Random (mkStdGen)
import Web.Browser (openBrowser)

-- | Main function. Plug in `serve` (for playing in the browser) or `compete` here, depending on what mode you want the program to start in.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["evaluate"] ->
      evaluate
    ["display"] ->
      serve "display.html"
    _ -> serve "index.html"

-- | Runs the server application.
serve :: String -> IO ()
serve file =
  let url = "http://localhost:8000/"
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
              | rawPathInfo request == "/ai/minimax" ->
                case decode $ fromStrict body of
                  Just state ->
                    responseLBS
                      status200
                      [("Content-Type", "text/plain")]
                      ( Ai.minimax 3 (mkStdGen 136) state |> fmap (result state)
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

displayedState :: State
displayedState =
  State
    { lastPlayer = Just Silver,
      player = (Gold, Nothing),
      gold =
        ( Just (Coordinate {x = 4, y = 4}),
          [ Coordinate {x = 9, y = 8},
            Coordinate {x = 3, y = 4},
            Coordinate {x = 5, y = 6},
            Coordinate {x = 8, y = 9},
            Coordinate {x = 6, y = 9},
            Coordinate {x = 1, y = 4},
            Coordinate {x = 3, y = 3},
            Coordinate {x = 7, y = 5},
            Coordinate {x = 5, y = 3},
            Coordinate {x = 6, y = 3},
            Coordinate {x = 4, y = 7},
            Coordinate {x = 5, y = 7}
          ]
        ),
      silver =
        [ Coordinate {x = 10, y = 9},
          Coordinate {x = 3, y = 2},
          Coordinate {x = 8, y = 2},
          Coordinate {x = 2, y = 9},
          Coordinate {x = 2, y = 7},
          Coordinate {x = 7, y = 9},
          Coordinate {x = 3, y = 7},
          Coordinate {x = 1, y = 3},
          Coordinate {x = 1, y = 5},
          Coordinate {x = 1, y = 6},
          Coordinate {x = 9, y = 5},
          Coordinate {x = 9, y = 6},
          Coordinate {x = 4, y = 1},
          Coordinate {x = 5, y = 1},
          Coordinate {x = 6, y = 1},
          Coordinate {x = 7, y = 1},
          Coordinate {x = 4, y = 9},
          Coordinate {x = 5, y = 9}
        ]
    }
{-# LANGUAGE TypeApplications #-}

module Main where

import Api (api)
import App (server)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import System.Environment (lookupEnv)

-- heroku provides PORT
readPort :: IO Int
readPort = do
  maybe 8080 (read @Int) <$> lookupEnv "PORT"

main :: IO ()
main = do
  putStrLn "Starting backend"
  port <- readPort
  run port . serve api $ server

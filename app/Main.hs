module Main where

import Api (api)
import App (server)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

main :: IO ()
main = do
  putStrLn "Starting backend"
  run 8080 . serve api $ server

{-# LANGUAGE TypeApplications #-}

module Main where

import Api (api)
import App (server)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
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

database :: IO (Maybe ByteString)
database =
  encodeUtf8 . T.pack . (<> "?sslMode=Require") . ("postgresql://" <>) <$$> lookupEnv "DATABASE_URL"

(<$$>) :: (a -> b) -> IO (Maybe a) -> IO (Maybe b)
(<$$>) = fmap . fmap

infixr 0 <$$>

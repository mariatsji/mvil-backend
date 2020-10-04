{-# LANGUAGE TypeApplications #-}

module Main where

import Api (api)
import App (server)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple
  ( connectPostgreSQL,
    withTransaction,
  )
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (MigrationDirectory, MigrationInitialization),
    MigrationContext (MigrationContext),
    runMigration,
  )
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import System.Environment (lookupEnv)
import System.IO
    ( stdout, hSetBuffering, BufferMode(LineBuffering) )

-- heroku provides PORT
readPort :: IO Int
readPort = do
  maybe 8080 (read @Int) <$> lookupEnv "PORT"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Starting Mårhunden backend"
  port <- readPort
  db <- database
  print db
  maybe (print "no database info found, so no migrations") migrations db
  run port . serve api $ server

database :: IO (Maybe ByteString)
database =
  encodeUtf8 . T.pack <$$> lookupEnv "DATABASE_URL"

(<$$>) :: (a -> b) -> IO (Maybe a) -> IO (Maybe b)
(<$$>) = fmap . fmap

infixr 0 <$$>

migrations :: ByteString -> IO ()
migrations connectInfo = do
  let dir = "./migrations/"
  con <- connectPostgreSQL connectInfo
  res <-
    withTransaction con (runMigration $ MigrationContext MigrationInitialization False con)
      >> withTransaction
        con
        (runMigration $ MigrationContext (MigrationDirectory dir) False con)
  print "db migrations ended one way or the other.. :"
  print res

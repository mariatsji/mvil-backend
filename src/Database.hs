{-# LANGUAGE OverloadedStrings #-}

module Database (insert, fetch) where

import Data.Int (Int64)
import Database.PostgreSQL.Simple ( execute, query_, Connection )
import ForumPost (ForumPost (..))

insert :: Connection -> ForumPost -> IO Int64
insert conn post =
  execute conn "insert into posts (\"user\", post, timestamp) values (?,?,?)" post

fetch :: Connection -> IO [ForumPost]
fetch conn = do
    xs <- query_ conn "select \"user\", post, timestamp from posts ORDER BY timestamp desc"
    pure $ (\(u,p,tz) -> ForumPost u p tz) <$> xs

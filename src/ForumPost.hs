{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ForumPost where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)

data ForumPost = ForumPost
  { author :: Text,
    body :: Text,
    timestamp :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow, ToRow)

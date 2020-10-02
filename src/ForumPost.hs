{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ForumPost where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)

data ForumPost = ForumPost
  { author :: Text,
    body :: Text,
    timestamp :: UTCTime
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

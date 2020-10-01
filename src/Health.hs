{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Health(Health(..)) where

import Data.Aeson ( ToJSON, FromJSON )
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Health = Health { status :: Text } deriving (Eq, Show, Generic, FromJSON, ToJSON)
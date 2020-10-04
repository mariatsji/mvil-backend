{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import ForumPost (ForumPost)
import Health (Health)
import Servant
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

newtype ApiKey = ApiKey Text deriving (Eq, Show)

type API 
  =    "health" :> Get '[JSON] Health
  :<|> "posts" :> Header "x-client-id" ApiKey :> Get '[JSON] [ForumPost]
  :<|> "posts" :> Header "x-client-id" ApiKey :> ReqBody '[JSON] ForumPost :> Post '[JSON] ForumPost
  :<|> "posts" :> "admin" :> Header "x-client-id" ApiKey :> Delete '[JSON] Text

api :: Proxy API
api = Proxy

instance FromHttpApiData ApiKey where
   parseHeader = Right . ApiKey . decodeUtf8
   parseUrlPiece = Right . ApiKey
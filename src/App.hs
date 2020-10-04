{-# LANGUAGE OverloadedStrings #-}

module App where

import Api
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime)
import Database (fetch, insert, trunc)
import Database.PostgreSQL.Simple (Connection)
import ForumPost (ForumPost (ForumPost))
import Health (Health (Health))
import Servant

server :: Maybe Connection -> ServerT API Handler
server mCon = health :<|> authed fetchUsing mCon :<|> authed2 insertUsing mCon :<|> authed truncateUsing mCon

health :: Handler Health
health = pure $ Health "ok"

authed2 :: (Maybe Connection -> a -> Handler [a]) -> Maybe Connection -> Maybe ApiKey -> a -> Handler [a]
authed2 _ _ Nothing _ = throwError $ err403 {errBody = "no access without x-client-key header"}
authed2 f mCon k a = authed (`f` a) mCon k

authed :: (Maybe Connection -> Handler a) -> Maybe Connection -> Maybe ApiKey -> Handler a
authed _ _ Nothing = throwError $ err403 {errBody = "no access without x-client-key header"}
authed f mCon k
  | k `elem` accessList = f mCon
  | otherwise = throwError $ err403 {errBody = "no access with invalid x-client-key header"}

truncateUsing :: Maybe Connection -> Handler Text
truncateUsing Nothing = pure "No database"
truncateUsing (Just conn) = liftIO $ trunc conn >> pure "truncated"

insertUsing :: Maybe Connection -> ForumPost -> Handler [ForumPost]
insertUsing Nothing p = pure [p]
insertUsing (Just con) p = liftIO $ insert con p >> fetch con

fetchUsing :: Maybe Connection -> Handler [ForumPost]
fetchUsing Nothing = do
  now <- liftIO getCurrentTime
  pure $ dummyPosts now
fetchUsing (Just con) = liftIO $ fetch con

accessList :: [Maybe ApiKey]
accessList =
  Just . ApiKey
    <$> [ "39668434-19fa-490c-8a44-370025b8453b",
          "577c7db6-f942-4ebd-a5d5-0483a7ee3c70",
          "47e3db3b-a32e-42e2-8ec1-746e6c36e186",
          "03fc053e-ea56-404a-9cc0-913fc6440508",
          "fd0b55f7-b479-4948-ac0c-0831650ea619"
        ]

requestHeaders :: t0 -> t
requestHeaders = error "not implemented"

dummyPosts :: UTCTime -> [ForumPost]
dummyPosts time =
  [ ForumPost
      "Sjur Millidahl"
      "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
      time,
    ForumPost
      "Noen Andre"
      "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
      time,
    ForumPost
      "Caramba Damba"
      "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
      time
  ]

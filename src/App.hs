{-# LANGUAGE OverloadedStrings #-}
module App where

import Health ( Health(Health) )
import Servant
import Api (API)

server :: ServerT API Handler
server = pure $ Health "ok"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Health (Health)
import Servant (Get, JSON, Proxy (..), type (:>))

type API =
  "health" :> Get '[JSON] Health

api :: Proxy API
api = Proxy

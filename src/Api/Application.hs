{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Application where

import Api.Docs (DocsAPI, docsServer)
import Api.Healthcheck (HealthcheckAPI, healthcheckServer)
import Api.Tagger (TaggerAPI, taggerServer)
import Tagger.ContentRepository (ContentRepository)
import Tagger.User (User)

-- base
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)

-- jose
import Crypto.JOSE.JWK (JWK)

-- servant
import Servant.API (NamedRoutes, type (:>))
import Servant.API.Generic ((:-))

-- servant-auth
import Servant.Auth (Auth, JWT)

-- servant-auth-server
import Servant.Auth.Server (AuthResult, defaultJWTSettings, defaultCookieSettings)

-- servant-server
import Servant (Handler, serveWithContext, Context ((:.), EmptyContext))
import Servant.Server.Generic (AsServer)

-- wai
import Network.Wai (Application)

-- wai-extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

type API = NamedRoutes ApplicationAPI

data ApplicationAPI mode = ApplicationAPI
  { tagger      :: mode :- Auth '[JWT] User :> NamedRoutes TaggerAPI
  , docs        :: mode :- DocsAPI
  , healthcheck :: mode :- HealthcheckAPI
  }
  deriving stock Generic

server :: ContentRepository Handler -> ApplicationAPI AsServer
server contentRepository = ApplicationAPI
  { tagger      = \(_ :: AuthResult User) -> taggerServer contentRepository
  , docs        = docsServer
  , healthcheck = healthcheckServer
  }

app :: JWK -> ContentRepository Handler -> Application
app key contentRepository = logStdoutDev $ serveWithContext
  (Proxy :: Proxy API)
  (defaultCookieSettings :. defaultJWTSettings key :. EmptyContext)
  (server contentRepository)

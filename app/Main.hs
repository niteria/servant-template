{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Application (app)
import Api.AppServices (AppServices(..))
import qualified Infrastructure.Authentication.AuthenticateUser as Auth (AuthenticateUser(AuthenticateUser), AuthenticationError(..), authenticateUser, hoistAuthenticateUser)
import Infrastructure.Authentication.PasswordManager (PasswordManager, PasswordManagerError(..), bcryptPasswordManager, hoistPasswordManager)
import Infrastructure.Persistence.PostgresContentRepository (postgresContentRepository)
import Infrastructure.Persistence.PostgresUserRepository (postgresUserRepository)
import Tagger.ContentRepository (ContentRepository, hoistContentRepository)
import Tagger.UserRepository (UserRepository, hoistUserRepository)

-- base
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

-- bytestring
import Data.ByteString.Char8 (unpack)

-- hasql
import Hasql.Connection (Connection, acquire)

-- jose
import Crypto.JOSE.JWK (JWK)

-- mtl
import Control.Monad.Except (throwError)

-- servant
import Servant (Handler, err401, err500)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings, defaultJWTSettings, generateKey)

-- transformers
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- warp
import Network.Wai.Handler.Warp (run)

eitherTToHandler :: (e -> Handler a) -> ExceptT e IO a -> Handler a
eitherTToHandler handleError = either handleError pure <=< liftIO . runExceptT

-- TODO: the whole hoist stuff could probably be managed in a better way
connectedContentRepository :: Connection -> ContentRepository Handler
connectedContentRepository = hoistContentRepository (eitherTToHandler . const $ throwError err500) . postgresContentRepository

connectedUserRepository :: Connection -> UserRepository Handler
connectedUserRepository = hoistUserRepository (eitherTToHandler . const $ throwError err500). postgresUserRepository

connectedAuthenticateUser :: Connection -> Auth.AuthenticateUser Handler
connectedAuthenticateUser = Auth.hoistAuthenticateUser (eitherTToHandler handleAuthenticationError) . Auth.AuthenticateUser . Auth.authenticateUser
  where
    handleAuthenticationError :: Auth.AuthenticationError -> Handler a
    handleAuthenticationError (Auth.AuthenticationQueryError _) = throwError err500
    handleAuthenticationError _                                 = throwError err401

encryptedPasswordManager :: JWTSettings -> PasswordManager Handler
encryptedPasswordManager = hoistPasswordManager (eitherTToHandler handlePasswordManagerError) . bcryptPasswordManager
  where
    handlePasswordManagerError :: PasswordManagerError -> Handler a
    handlePasswordManagerError FailedHashing         = throwError err500
    handlePasswordManagerError (FailedJWTCreation _) = throwError err401

appServices :: Connection -> JWK -> AppServices
appServices connection key = AppServices
  { jwtSettings       = defaultJWTSettings key
  , passwordManager   = encryptedPasswordManager $ defaultJWTSettings key
  , contentRepository = connectedContentRepository connection
  , userRepository    = connectedUserRepository connection
  , authenticateUser  = connectedAuthenticateUser connection
  }

main:: IO ()
main = do
  -- TODO: retrieve connection data from configuration file
  connection <- acquire "host=localhost port=5432 dbname=tagger user=user password=password"
  key <- generateKey
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (\connection' -> run 8080 $ app (appServices connection' key))
    connection

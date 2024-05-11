module Tagger.JSONWebKey
  ( setup
  , JWK
  ) where

import CLIOptions (CLIOptions(jwkPath))
import Control.Exception (catch)
import Crypto.JOSE.JWK (JWK)
import Data.ByteString.Char8 (writeFile)
import Prelude hiding (writeFile)
import Servant.Auth.Server (fromSecret, generateSecret, readKey)

setup :: CLIOptions -> IO JWK
setup config = do
  let path = jwkPath config
  -- try to retrieve the JWK from file
  catch (readKey path) $ \(_ :: IOError)
    -- if the file does not exist or does not contain a valid key, we generate one
   -> do
    key <- generateSecret
    -- and we store it
    writeFile path key
    pure $ fromSecret key

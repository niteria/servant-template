{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infrastructure.Authentication.Token where

-- base
import Data.Data (Proxy(Proxy))

-- aeson
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(String), withText)

-- bytestring
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)

-- openapi3
import Data.OpenApi (ToSchema(declareNamedSchema))

-- text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

newtype Token = Token ByteString
  deriving newtype Show

instance FromJSON Token where
  parseJSON = withText "Token" (pure . Token . fromStrict .  encodeUtf8)

instance ToJSON Token where
  toJSON (Token bs) = String . decodeUtf8 $ toStrict bs

instance ToSchema Token where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

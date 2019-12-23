{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LolHub.Domain.User where

import           Prelude hiding (exp)
import           Data.Data (Typeable)
import           GHC.Generics
import           Data.Bson.Mapping
import           Database.MongoDB (ObjectId)
import           Data.Text
import           Data.Aeson (encode, decode, ToJSON, FromJSON, Value)
import           Data.Time.Clock.POSIX
import qualified Web.JWT as JWT
import qualified Data.Map as Map
import Data.Time.Clock

data User = User { _id :: ObjectId
                 , username :: String
                 , email :: String
                 , firstname :: String
                 , lastname :: String
                 , password :: String
                 , token :: Text
                 }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''User)

data Session = Session { uname :: Text, iat :: NominalDiffTime, exp :: NominalDiffTime }
  deriving (Generic, Typeable, Show, Eq, Ord)

instance ToJSON Session

instance FromJSON Session

encodeSession :: Session -> Text
encodeSession session =
  let claims = decode $ encode $ session :: Maybe (Map.Map Text Value)
      cs = mempty   -- mempty returns a default JWTClaimsSet
        { JWT.iss = JWT.stringOrURI "LolHub"
        , JWT.iat = JWT.numericDate $ iat session
        , JWT.exp = JWT.numericDate $ exp session-- TODO: when we have a URI insert here
        , JWT.unregisteredClaims = JWT.ClaimsMap
            -- $ Map.fromList [("http://example.com/is_root", (Bool True))]
            $ case claims of
              Just m  -> m
              Nothing -> Map.fromList []
        }
      key = JWT.hmacSecret "secret-key"
  in JWT.encodeSigned key mempty cs
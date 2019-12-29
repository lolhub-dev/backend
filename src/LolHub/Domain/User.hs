{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LolHub.Domain.User where

import           Prelude hiding (exp, reverse)
import           Data.Data (Typeable)
import           GHC.Generics
import           Data.Bson.Mapping
import           Database.MongoDB (ObjectId)
import           Data.Text
import           Data.Aeson (encode, decode, ToJSON, FromJSON, Value)
import           Data.Time.Clock.POSIX
import qualified Web.JWT as JWT
import qualified Data.Map.Strict as Map
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (POSIXTime)

data UserE = UserE { _id :: ObjectId
                   , username :: Text
                   , email :: Text
                   , firstname :: Text
                   , lastname :: Text
                   , password :: Text
                   , token :: Text
                   }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''UserE)

data SessionE =
  SessionE { uname :: Text, iat :: NominalDiffTime, exp :: NominalDiffTime }
  deriving (Generic, Typeable, Show, Eq, Ord)

instance ToJSON SessionE

instance FromJSON SessionE

secret :: Text
secret = "TVwTQvknx0vaQE6mTlFJPB9VSbz5iPRS"

createSession :: Text -> POSIXTime -> SessionE
createSession username currTime =
  SessionE { uname = username
           , iat = currTime
           , exp = currTime + 1000 -- //TODO: declare offset here" 
           }

decodeSession :: Text -> Maybe SessionE
decodeSession token =
  let key = JWT.hmacSecret secret
  in do
       mJwt <- return $ JWT.decodeAndVerifySignature key token
       claims <- fmap JWT.claims mJwt
       unregisteredMap
         <- return $ JWT.unClaimsMap $ JWT.unregisteredClaims claims
       uname <- pack <$> (show <$> (Map.lookup "uname" unregisteredMap))
       parsed <- stripPrefix "String \""
         $ reverse
         $ pack ((\(x1:xs) -> xs) $ unpack (reverse uname))
       iat <- JWT.iat claims
       exp <- JWT.exp claims
       return
         SessionE { uname = parsed
                  , iat = JWT.secondsSinceEpoch iat
                  , exp = JWT.secondsSinceEpoch exp
                  }

encodeSession :: SessionE -> Text
encodeSession session =
  let claims = decode $ encode $ session :: Maybe (Map.Map Text Value) -- hacky way to parse native haskell type into Map of Text and Value
      cs = mempty   -- mempty returns a default JWTClaimsSet
        { JWT.iss = JWT.stringOrURI "LolHub" -- //TODO: read that from env file or so 
        , JWT.iat = JWT.numericDate $ iat session
        , JWT.exp = JWT.numericDate $ exp session-- //TODO: when we have a URI insert here
        , JWT.unregisteredClaims = JWT.ClaimsMap
            $ case claims of
              Just m  -> m
              Nothing -> Map.fromList []
        }
      key = JWT.hmacSecret secret
  in JWT.encodeSigned key mempty cs
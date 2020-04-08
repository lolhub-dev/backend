{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module LolHub.Domain.User where

import           Control.Lens                   ( makeLenses )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , Value
                                                , decode
                                                , encode
                                                )
import           Data.Bson.Mapping
import           Data.Data                      ( Typeable )
import qualified Data.Map.Strict               as Map
import           Data.Text
import           Data.Time.Clock
import           Data.Time.Clock.POSIX          ( POSIXTime )
import           Database.MongoDB               ( ObjectId )
import           Debug.Trace                    ( traceId )
import           GHC.Generics
import           Prelude                 hiding ( exp
                                                , reverse
                                                )
import qualified Web.JWT                       as JWT


data VerificationStatusE = UNVERIFIED | VERIFIED | SUMMONER_VERIFIED
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''VerificationStatusE)

$(makeLenses ''VerificationStatusE)

data UserE = UserE { _id        :: ObjectId
                   , _username  :: Text
                   , _email     :: Text
                   , _firstname :: Text
                   , _lastname  :: Text
                   , _password  :: Text
                   , _token     :: Text
                   , _verified  :: VerificationStatusE
                   }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''UserE)

$(makeLenses ''UserE)


data SessionE =
  SessionE { _uname :: Text, _iat :: NominalDiffTime, _exp :: NominalDiffTime }
  deriving (Generic, Typeable, Show, Eq, Ord)

instance ToJSON SessionE

instance FromJSON SessionE

$(makeLenses ''SessionE)

secret :: Text
secret = "TVwTQvknx0vaQE6mTlFJPB9VSbz5iPRS"

createSession :: Text -> POSIXTime -> SessionE
createSession username currTime =
        SessionE { _uname = username, _iat = currTime, _exp = currTime + 1000 }

decodeSession :: Text -> Maybe SessionE
decodeSession token =
        let key = JWT.hmacSecret secret
        in
                do
                        let mJwt = JWT.decodeAndVerifySignature key token
                        claims <- fmap JWT.claims mJwt
                        let
                                unregisteredMap = JWT.unClaimsMap
                                        $ JWT.unregisteredClaims claims
                        uname <-
                                pack
                                        <$> (   show
                                            <$> Map.lookup
                                                        "_uname"
                                                        unregisteredMap
                                            )
                        parsed <- stripPrefix "String \"" $ reverse $ pack
                                ((\(x1 : xs) -> xs) $ unpack (reverse uname))
                        iat <- JWT.iat claims
                        exp <- JWT.exp claims
                        return SessionE { _uname = parsed
                                        , _iat   = JWT.secondsSinceEpoch iat
                                        , _exp   = JWT.secondsSinceEpoch exp
                                        }

encodeSession :: SessionE -> Text
encodeSession session =
        let
                claims = decode $ encode session :: Maybe (Map.Map Text Value) -- hacky way to parse native haskell type into Map of Text and Value
                cs     = mempty   -- mempty returns a default JWTClaimsSet
                        { JWT.iss = JWT.stringOrURI "LolHub" -- //TODO: read that from env file or so
                        , JWT.iat = JWT.numericDate $ _iat session
                        , JWT.exp = JWT.numericDate $ _exp session-- //TODO: when we have a URI insert here
                        , JWT.unregisteredClaims =
                                JWT.ClaimsMap $ case claims of
                                        Just m  -> m
                                        Nothing -> Map.fromList []
                        }
                key = JWT.hmacSecret secret
        in
                JWT.encodeSigned key mempty cs

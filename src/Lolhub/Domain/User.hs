{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LolHub.Domain.User where

import           Data.Data (Typeable)
import           GHC.Generics
import           Data.Bson.Mapping
import           Database.MongoDB (ObjectId)
import           Data.Text
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Encoding
import           Data.Aeson
import           Data.Time.Clock.POSIX
import           Data.Aeson (encode, decode)
import           Web.JWT
import qualified Data.Map as Map

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

data Session = Session { uname :: Text, timestamp :: Integer }
  deriving (Show, Generic)

instance ToJSON Session

instance FromJSON Session

encodeSession :: Session -> Text
encodeSession session =
  let cs = mempty   -- mempty returns a default JWTClaimsSet
        { iss = stringOrURI Foo
        , unregisteredClaims = Map.fromList
            [("http://example.com/is_root", (Bool True))]
        }
      key = hmacSecret "secret-key"
  in encodeSigned key mempty cs
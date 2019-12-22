{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lolhub.Domain.User where

import           Data.Data (Typeable)
import           GHC.Generics
import           Data.Bson.Mapping
import           Data.Text
import           Data.Aeson

data User = User { id :: Integer
                 , username :: String
                 , email :: String
                 , firstname :: String
                 , lastname :: String
                 , password :: String
                 , token :: Text
                 }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''User)

data Session = Session { sessionId :: Integer
                       , date :: Text
                       , time :: Text
                       , validUntil :: Text
                       }
  deriving (Show, Generic)

instance ToJSON Session

instance FromJSON Session
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LolHub.Domain.SummonerToken where

import           Data.Text (Text)
import           Database.MongoDB (ObjectId)
import           Data.Data (Typeable)
import           GHC.Generics
import           Data.Bson.Mapping

data SummonerTokenE = SummonerTokenE { name :: Text, token :: Text }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''SummonerTokenE)

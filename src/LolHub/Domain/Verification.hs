{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LolHub.Domain.Verification where

import           Data.Text
import           Database.MongoDB (ObjectId)
import           Data.Data (Typeable)
import           GHC.Generics
import           Data.Bson.Mapping

data VerificationE = {
    _id :: ObjectId,
    username :: Text,
    token :: Text
}

$(deriveBson ''VerificationE)

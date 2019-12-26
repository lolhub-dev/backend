{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

module LolHub.Graphql.Query.UserQuery where

import           LolHub.Graphql.Types
import           GHC.Generics
import           Data.Text
import           Data.Morpheus.Types (GQLType, MutRes)

data Query m = Query { queryHelloWorld :: () -> m Text }
  deriving (Generic, GQLType)

data Mutation m = Mutation { mutationRegister :: MutationRegisterArgs
                                              -> m (User (MutRes USEREVENT IO))
                           , mutationLogin :: MutationLoginArgs
                                           -> m (User (MutRes USEREVENT IO))
                           }
  deriving (Generic, GQLType)

data MutationRegisterArgs =
  MutationRegisterArgs { mutationRegisterArgsUsername :: Text
                       , mutationRegisterArgsFirstname :: Text
                       , mutationRegisterArgsLastname :: Text
                       , mutationRegisterArgsEmail :: Text
                       , mutationRegisterArgsPassword :: Text
                       }
  deriving (Generic)

data MutationLoginArgs = MutationLoginArgs { mutationLoginArgsUsername :: Text
                                           , mutationLoginArgsPassword :: Text
                                           }
  deriving (Generic)
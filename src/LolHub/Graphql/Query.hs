{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving #-}

module LolHub.Graphql.Query where

import           LolHub.Graphql.Types
import           GHC.Generics
import           Data.Morpheus.Types (GQLType(..))

data Query m = Query { queryHelloWorld :: () -> m String }
  deriving (Generic, GQLType)

data Mutation m = Mutation { mutationRegister :: MutationRegisterArgs -> m User
                           , mutationLogin :: MutationLoginArgs -> m User
                           }
  deriving (Generic, GQLType)

data MutationRegisterArgs =
  MutationRegisterArgs { mutationRegisterArgsUsername :: String
                       , mutationRegisterArgsFirstname :: String
                       , mutationRegisterArgsLastname :: String
                       , mutationRegisterArgsEmail :: String
                       , mutationRegisterArgsPassword :: String
                       }
  deriving (Generic, GQLType)

data MutationLoginArgs = MutationLoginArgs { mutationLoginArgsUsername :: String
                                           , mutationLoginArgsPassword :: String
                                           }
  deriving (Generic, GQLType)
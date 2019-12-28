{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module LolHub.Graphql.Query.UserQuery where

import           LolHub.Graphql.Types
import           GHC.Generics
import           Data.Text
import           Data.Morpheus.Types (GQLType, MutRes)

data Query m = Query { helloWorld :: () -> m Text }
  deriving (Generic, GQLType)

data Mutation m =
  Mutation { register :: RegisterArgs -> m (User (MutRes USEREVENT IO))
           , login :: LoginArgs -> m (User (MutRes USEREVENT IO))
           }
  deriving (Generic, GQLType)

data RegisterArgs = RegisterArgs { username :: Text
                                 , firstname :: Text
                                 , lastname :: Text
                                 , email :: Text
                                 , password :: Text
                                 }
  deriving (Generic)

data LoginArgs = LoginArgs { username :: Text, password :: Text }
  deriving (Generic)
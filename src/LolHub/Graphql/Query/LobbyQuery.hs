{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

module LolHub.Graphql.Query.LobbyQuery where

import           LolHub.Graphql.Types
import           GHC.Generics
import           Data.Text
import           Data.Morpheus.Types (GQLType(..), MutRes)

data Query m = Query { helloWorld :: () -> m Text }
  deriving (Generic, GQLType)

data Mutation m = Mutation { createLobby :: MutationCreateLobbyArgs
                                         -> m (Lobby (MutRes USEREVENT IO))
                           }
  deriving (Generic, GQLType)

data MutationCreateLobbyArgs = MutationCreateLobbyArgs { kind :: LobbyKind }
  deriving (Generic)
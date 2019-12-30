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

module LolHub.Graphql.Query.LobbyQuery where

import           LolHub.Graphql.Types
import           GHC.Generics
import           Data.Text
import           Data.Morpheus.Types (GQLType(..), IOMutRes, IOSubRes, IORes)

data Query m = Query { helloWorld :: () -> m Text }
  deriving (Generic, GQLType)

data Mutation m =
  Mutation { create :: CreateLobbyArgs -> m (Lobby (IOMutRes USEREVENT))
           , join :: JoinLobbyArgs -> m (Lobby (IOMutRes USEREVENT))
           }
  deriving (Generic, GQLType)

data Subscription m =
  Subscription { joined :: JoinedLobbyArgs -> m (UserJoined (IORes USEREVENT))
               }
  deriving (Generic, GQLType)

data JoinedLobbyArgs = JoinedLobbyArgs { _id :: Text }
  deriving (Generic)

data JoinLobbyArgs = JoinLobbyArgs { _id :: Text }
  deriving (Generic)

data CreateLobbyArgs = CreateLobbyArgs { kind :: LobbyKind }
  deriving (Generic)
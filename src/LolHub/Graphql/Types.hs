{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module LolHub.Graphql.Types where

import qualified LolHub.Domain.User as User
import qualified LolHub.Domain.Lobby as Lobby
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), Resolver)
import           Data.Morpheus.Types.Internal.AST
import           Database.MongoDB (ObjectId)
import           Data.Text
import           Control.Concurrent.MonadIO

importGQLDocumentWithNamespace "src/LolHub/Graphql/Types.gql"

type Leaf (o :: OperationType) a = Resolver o () a

type Graphql (o :: OperationType) a = Resolver o () (a (Resolver o () IO))

data Channel = USER
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int }

type USEREVENT = (Event Channel Content)
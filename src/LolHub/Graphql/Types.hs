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

import           Data.Text (Text)
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), Resolver, WithOperation)
import           Data.Morpheus.Types.Internal.AST (OperationType)
import           Control.Concurrent.MonadIO (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans)

importGQLDocumentWithNamespace "src/LolHub/Graphql/Types.gql"

-- | Resolve single value
--
type Value (o :: OperationType) a = Resolver o () IO a

-- | Resolve object (which includes other fields that need their own resolvers)
--
type Object (o :: OperationType) a = Resolver o () IO (a (Resolver o () IO))

-- | Resolve (Maybe object)
--
type OptionalObject (o :: OperationType) a =
  Resolver o () IO (Maybe (a (Resolver o () IO)))

-- | Resolve (Either Error object)
--
type EitherObject (o :: OperationType) a b =
  Either a (Resolver o () IO (b (Resolver o () IO)))

-- | Resolve [object]
--
type ArrayObject (o :: OperationType) a =
  Resolver o () IO [a (Resolver o () IO)]

type GraphQL o =
  (MonadIO (Resolver o () IO), WithOperation o, MonadTrans (Resolver o ()))

data Channel = USER
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int }

type USEREVENT = (Event Channel Content)
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}


module LolHub.Graphql.Types where

import           Control.Monad.Trans.Class        (MonadTrans)
import           Data.Morpheus.Document           (importGQLDocument)
import           Data.Morpheus.Types              (Event (..), Resolver,
                                                   WithOperation)
import           Data.Morpheus.Types.Internal.AST (OperationType)
import           Data.Text                        (Text)

importGQLDocument "src/LolHub/Graphql/Types.gql"


type Value (o :: OperationType) a e = Resolver o e IO a

-- | Resolve object (which includes other fields that need their own resolvers)
--
type Object (o :: OperationType) e a = a (Resolver o e IO)

-- | Resolve (Maybe object)
--
type OptionalObject (o :: OperationType) e a = Maybe (Object o e a)

-- | Resolve (Either Error object)
--
type EitherObject (o :: OperationType) e a b = Either a (Object o e b)

data Channel = USER
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int }

type USEREVENT = (Event Channel Content)

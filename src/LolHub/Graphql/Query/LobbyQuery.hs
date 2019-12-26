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

module LolHub.Graphql.Query.LobbyQuery where

import           LolHub.Graphql.Types
import           GHC.Generics
import           Data.Text
import           Data.Morpheus.Types (GQLType(..))

data LobbyQuery m = LobbyQuery { queryHelloWorld :: () -> m Text }
  deriving (Generic, GQLType)
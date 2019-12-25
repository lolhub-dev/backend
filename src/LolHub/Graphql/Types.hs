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

module LolHub.Graphql.Types where

import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Text

importGQLDocumentWithNamespace "src/LolHub/Graphql/Api.gql"

importGQLDocumentWithNamespace "src/LolHub/Graphql/Query.gql"

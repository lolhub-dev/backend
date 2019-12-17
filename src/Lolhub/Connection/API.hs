{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Lolhub.Connection.API
  ( api
  )
where

import qualified Data.ByteString.Lazy.Char8    as B

import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocumentWithNamespace
                                                )
import           Data.Morpheus.Types            ( GQLRootResolver(..)
                                                , IORes
                                                )
import           Data.Text                      ( Text )

importGQLDocumentWithNamespace "schema.gql"

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver { queryResolver        = Query { queryDeity }
                               , mutationResolver     = Undefined
                               , subscriptionResolver = Undefined
                               }
 where
  queryDeity QueryDeityArgs { queryDeityArgsName } = pure Deity { deityName
                                                                , deityPower
                                                                }
   where
    deityName _ = pure "Morpheus"
    deityPower _ = pure (Just "Shapeshifting")

api :: ByteString -> IO ByteString
api = interpreter rootResolver

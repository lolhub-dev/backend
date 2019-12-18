{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Lolhub.Connection.API
  ( gqlApi
  , EVENT
  )
where

import qualified Data.ByteString.Lazy.Char8    as B

import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocumentWithNamespace
                                                )
import           Data.Morpheus.Types            ( GQLRootResolver(..)
                                                , Undefined(..)
                                                , ResolveQ
                                                , ResolveM
                                                , ResolveS
                                                , IORes
                                                , Event(..)
                                                , constRes
                                                , liftEither
                                                )
import           Data.Text                      ( Text )

importGQLDocumentWithNamespace "src/schema.gql"

data Channel = USER | ADDRESS
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int  }

type EVENT = (Event Channel Content)

gqlApi :: B.ByteString -> IO B.ByteString
gqlApi = interpreter gqlRoot

gqlRoot :: GQLRootResolver IO EVENT Query Undefined Undefined
gqlRoot = GQLRootResolver { queryResolver
                          , mutationResolver
                          , subscriptionResolver
                          }
 where
  queryResolver =
    Query { queryUser = resolveUser, queryHelloWorld = resolveHelloWorld }
  -------------------------------------------------------------
  mutationResolver     = Undefined
  subscriptionResolver = Undefined


----- QUERY RESOLVERS -----

resolveUser :: () -> ResolveQ EVENT IO User
resolveUser _args = liftEither (getDBUser (Content 2))

resolveHelloWorld :: () -> IORes EVENT Text
resolveHelloWorld = constRes "helloWorld"

getDBUser :: Content -> IO (Either String (User (IORes EVENT)))
getDBUser _ = do
  Person { name, email } <- dbPerson
  pure $ Right User { userName = constRes name, userEmail = constRes email }

data Person = Person {
  name :: Text,
  email :: Text
}

dbPerson :: IO Person
dbPerson = pure Person { name = "George", email = "George@email.com" }

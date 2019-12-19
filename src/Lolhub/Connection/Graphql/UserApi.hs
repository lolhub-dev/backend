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

module Lolhub.Connection.Graphql.UserApi
  ( userApi
  , USEREVENT
  )
where

import           Control.Monad.Trans            ( lift )
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
                                                , IOMutRes
                                                , Event(..)
                                                , constRes
                                                , liftEither
                                                )
import           Data.Text                      ( Text )

importGQLDocumentWithNamespace "src/Lolhub/Connection/Graphql/UserApi.gql"


data Channel = USER | ADDRESS
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int  }

type USEREVENT = (Event Channel Content)

userApi :: B.ByteString -> IO B.ByteString
userApi = interpreter userGqlRoot

userGqlRoot :: GQLRootResolver IO USEREVENT Query Mutation Undefined
userGqlRoot = GQLRootResolver { queryResolver
                              , mutationResolver
                              , subscriptionResolver
                              }
 where
  queryResolver =
    Query { queryLogin = loginUser, queryHelloWorld = resolveHelloWorld }
  -------------------------------------------------------------
  mutationResolver     = Mutation { mutationRegister = registerUser }
  subscriptionResolver = Undefined


----- QUERY RESOLVERS -----

loginUser :: QueryLoginArgs -> ResolveQ USEREVENT IO User
loginUser _args = liftEither (getDBUser (Content 2))

resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld"

----- MUTATION RESOLVERS
registerUser :: MutationRegisterArgs -> ResolveM USEREVENT IO User
registerUser _args = lift setDBUser



getDBUser :: Content -> IO (Either String (User (IORes USEREVENT)))
getDBUser _ = do
  Person { name, email } <- dbPerson
  return $ Right User { userName = constRes name, userEmail = constRes email }

setDBUser :: IO (User (IOMutRes USEREVENT))
setDBUser = do
  Person { name, email } <- dbPerson
  return User { userName = constRes name, userEmail = constRes email }

data Person = Person {
  name :: Text,
  email :: Text
}

dbPerson :: IO Person
dbPerson = return Person { name = "George", email = "George@email.com" }

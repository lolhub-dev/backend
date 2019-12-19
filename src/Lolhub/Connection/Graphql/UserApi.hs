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

module Lolhub.Connection.Graphql.UserApi (userApi, USEREVENT) where

import           Control.Monad.Trans (lift)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , Undefined(..), constRes, liftEither)
import           Data.Text (Text)

importGQLDocumentWithNamespace "src/Lolhub/Connection/Graphql/UserApi.gql"

data Channel = USER
             | ADDRESS
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int }

type USEREVENT = (Event Channel Content)

userApi :: B.ByteString -> IO B.ByteString
userApi = interpreter userGqlRoot

userGqlRoot :: GQLRootResolver IO USEREVENT Query Mutation Undefined
userGqlRoot =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  where
    queryResolver =
      Query { queryLogin = loginUser, queryHelloWorld = resolveHelloWorld }

    -------------------------------------------------------------
    mutationResolver = Mutation { mutationRegister = registerUser }

    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
loginUser :: QueryLoginArgs -> ResolveQ USEREVENT IO UnverifiedUser
loginUser _args = liftEither (getDBUser (Content 2))

resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld"

----- MUTATION RESOLVERS
registerUser :: MutationRegisterArgs -> ResolveM USEREVENT IO UnverifiedUser
registerUser _args = lift setDBUser

getDBUser :: Content -> IO (Either String (UnverifiedUser (IORes USEREVENT)))
getDBUser _ = do
  UnverifiedPerson { name, surname, email } <- dbPerson
  return
    $ Right
      UnverifiedUser { unverifiedUserName = constRes name
                     , unverifiedUserEmail = constRes email
                     , unverifiedUserSurname = constRes surname
                     }

setDBUser :: IO (UnverifiedUser (IOMutRes USEREVENT))
setDBUser = do
  UnverifiedPerson { name, surname, email } <- dbPerson
  return
    UnverifiedUser { unverifiedUserName = constRes name
                   , unverifiedUserEmail = constRes email
                   , unverifiedUserSurname = constRes surname
                   }

dbPerson :: IO UnverifiedPerson
dbPerson = return
  UnverifiedPerson { name = "George"
                   , surname = "Hotz"
                   , email = "George@email.com"
                   }

data UnverifiedPerson =
  UnverifiedPerson { name :: Text, surname :: Text, email :: Text }

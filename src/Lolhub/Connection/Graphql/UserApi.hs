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

import Database.MongoDB (Pipe)

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

userApi :: Pipe -> B.ByteString -> IO B.ByteString
userApi pipe = interpreter $ userGqlRoot pipe

userGqlRoot :: Pipe -> GQLRootResolver IO USEREVENT Query Mutation Undefined
userGqlRoot pipe =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  where
    queryResolver = Query { queryHelloWorld = resolveHelloWorld }

    -------------------------------------------------------------
    mutationResolver =
      Mutation { mutationLogin = loginUser, mutationRegister = registerUser }

    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
loginUser :: MutationLoginArgs -> ResolveM USEREVENT IO UnverifiedUser
loginUser
  MutationLoginArgs { mutationLoginArgsUsername, mutationLoginArgsPassword } =
  liftEither (getDBUser mutationLoginArgsUsername)

resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld"

----- MUTATION RESOLVERS -----
registerUser :: MutationRegisterArgs -> ResolveM USEREVENT IO UnverifiedUser
registerUser _args = lift setDBUser

----- STUB DB -----
getDBUser :: Text -> IO (Either String (UnverifiedUser (IOMutRes USEREVENT)))
getDBUser uname = do
  UnverifiedPerson { name, surname, email } <- dbPerson
  return
    $ if uname == "test"
      then Right
        UnverifiedUser { unverifiedUserName = constRes name
                       , unverifiedUserEmail = constRes email
                       , unverifiedUserSurname = constRes surname
                       }
      else Left "No such user"

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

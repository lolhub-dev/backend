{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module LolHub.Connection.Graphql.UserApi (userApi, USEREVENT) where

import           Control.Monad.Trans (lift)
import qualified Data.ByteString.Lazy.Char8 as B
import           Database.MongoDB (Pipe)
import qualified Data.Text as T
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , Undefined(..), constRes, liftEither)
import           Data.Text (Text)
import           LolHub.Connection.DB.Mongo (run)
import           LolHub.Connection.DB.Coll.User

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
    mutationResolver = Mutation { mutationLogin = loginUser pipe
                                , mutationRegister = registerUser pipe
                                }

    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
loginUser :: Pipe -> MutationLoginArgs -> ResolveM USEREVENT IO UnverifiedUser
loginUser
  pipe
  MutationLoginArgs { mutationLoginArgsUsername, mutationLoginArgsPassword } =
  liftEither (getDBUser pipe mutationLoginArgsUsername)

resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld"

----- MUTATION RESOLVERS -----
registerUser
  :: Pipe -> MutationRegisterArgs -> ResolveM USEREVENT IO UnverifiedUser
registerUser pipe args = lift (setDBUser pipe args)

----- STUB DB -----
getDBUser
  :: Pipe -> Text -> IO (Either String (UnverifiedUser (IOMutRes USEREVENT)))
getDBUser pipe uname = do
  result <- run (getUser $ T.unpack uname) pipe
  return
    $ case result of
      Nothing -> Left "No such user found"
      Just User { username, email, firstname, lastname, password } -> Right
        UnverifiedUser { unverifiedUserName = constRes $ T.pack firstname
                       , unverifiedUserSurname = constRes $ T.pack lastname
                       , unverifiedUserEmail = constRes $ T.pack email
                       }

setDBUser
  :: Pipe -> MutationRegisterArgs -> IO (UnverifiedUser (IOMutRes USEREVENT))
setDBUser
  pipe
  MutationRegisterArgs { mutationRegisterArgsUsername
                       , mutationRegisterArgsName
                       , mutationRegisterArgsSurname
                       , mutationRegisterArgsEmail
                       , mutationRegisterArgsPassword
                       } = do
  run (insertUser user) pipe
  return
    UnverifiedUser { unverifiedUserName = constRes $ T.pack $ firstname user
                   , unverifiedUserSurname = constRes $ T.pack $ lastname user
                   , unverifiedUserEmail = constRes $ T.pack $ email user
                   }
  where
    user = User { username = T.unpack mutationRegisterArgsUsername
                , email = T.unpack mutationRegisterArgsEmail
                , firstname = T.unpack mutationRegisterArgsName
                , lastname = T.unpack mutationRegisterArgsSurname
                , password = T.unpack mutationRegisterArgsPassword
                }

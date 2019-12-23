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
import           Database.MongoDB (Pipe, Failure, genObjectId)
import           Data.Text
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , Undefined(..), constRes, liftEither)
import           Data.Text (Text)
import           LolHub.Connection.DB.Mongo (run)
import qualified LolHub.Connection.DB.Coll.User as User
import           LolHub.Domain.User
import           Control.Monad.IO.Class (liftIO)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Aeson (encode, decode)

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
resolveHelloWorld = constRes "helloWorld" -- TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
registerUser
  :: Pipe -> MutationRegisterArgs -> ResolveM USEREVENT IO UnverifiedUser
registerUser pipe args = liftEither (setDBUser pipe args)

----- STUB DB -----
getDBUser
  :: Pipe -> Text -> IO (Either String (UnverifiedUser (IOMutRes USEREVENT)))
getDBUser pipe uname = do
  result <- run (User.getUserByName $ unpack uname) pipe
  print result
  return
    $ case result of
      Nothing -> Left "No such user found"
      Just
        User.User { User.username
                  , User.email
                  , User.firstname
                  , User.lastname
                  , User.password
                  } -> Right
        UnverifiedUser { unverifiedUserName = constRes $ pack firstname
                       , unverifiedUserSurname = constRes $ pack lastname
                       , unverifiedUserEmail = constRes $ pack email
                       }

setDBUser :: Pipe
          -> MutationRegisterArgs
          -> IO (Either String (UnverifiedUser (IOMutRes USEREVENT)))
setDBUser
  pipe
  MutationRegisterArgs { mutationRegisterArgsUsername
                       , mutationRegisterArgsName
                       , mutationRegisterArgsSurname
                       , mutationRegisterArgsEmail
                       , mutationRegisterArgsPassword
                       } = do
  objectId <- genObjectId
  currTime <- round <$> getPOSIXTime
  user <- return
    User.User { User._id = objectId
              , User.username = unpack mutationRegisterArgsUsername
              , User.email = unpack mutationRegisterArgsEmail
              , User.firstname = unpack mutationRegisterArgsName
              , User.lastname = unpack mutationRegisterArgsSurname
              , User.password = unpack mutationRegisterArgsPassword
              , User.token = encodeSession
                  Session { uname = mutationRegisterArgsUsername
                          , timestamp = currTime
                          }
              }
  eitherActionOrFailure <- User.insertUser user
  case eitherActionOrFailure of
    Right action -> liftIO
      $ do
        run action pipe
        return
          $ Right
            UnverifiedUser { unverifiedUserName =
                               constRes $ pack $ User.firstname user
                           , unverifiedUserSurname =
                               constRes $ pack $ User.lastname user
                           , unverifiedUserEmail =
                               constRes $ pack $ User.email user
                           }
    Left failure -> return $ Left $ show failure

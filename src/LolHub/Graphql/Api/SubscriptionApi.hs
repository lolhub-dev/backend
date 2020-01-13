{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}


module LolHub.Graphql.Api.SubscriptionApi where
import           Data.Morpheus.Document (importGQLDocument)
import           Data.Morpheus.Types    (Event (..), GQLRootResolver (..),
                                         IOMutRes, IORes, IOSubRes, MUTATION,
                                         QUERY, ResolveM, ResolveQ, ResolveS,
                                         Resolver (..), SUBSCRIPTION,
                                         Undefined (..), lift, liftEither)
import           Data.Text              (Text, pack, unpack)
import           LolHub.Graphql.Types

importGQLDocument "src/LolHub/Graphql/Query/Subscription.gql"


subscriptionRoot :: GQLRootResolver IO USEREVENT Query Undefined Subscription
subscriptionRoot = GQLRootResolver { queryResolver
                                   , mutationResolver
                                   , subscriptionResolver
                                   }
    where
        queryResolver        = Query { helloWorld = resolveHelloWorld }
        mutationResolver     = Undefined
        subscriptionResolver = Subscription { joined = resolveJoinedLobby }


resolveHelloWorld :: Value QUERY Text USEREVENT
resolveHelloWorld = return $ pack "helloWorld" -- //TODO: remove this, when there are other queries

resolveJoinedLobby :: joinedArgs -> ResolveS USEREVENT IO UserJoined
resolveJoinedLobby args = SubResolver { subChannels = [USER]
                                      , subResolver = subResolver
                                      }
    where
        subResolver (Event [USER] content) = lift (resolveJoinedLobby' content)

        resolveJoinedLobby' :: Content -> IO (Object QUERY USEREVENT UserJoined)
        resolveJoinedLobby' content = return UserJoined
                { username = return $ pack $ show $ contentID content
                }

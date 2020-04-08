{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Core.Network.Wai.Middleware.JWT
import           Data.Maybe
import           Data.Morpheus.Server           ( GQLState
                                                , gqlSocketApp
                                                , initGQLState
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Lazy                 ( toStrict )
import           Database.MongoDB               ( Action
                                                , Document
                                                , Pipe
                                                , access
                                                , close
                                                , connect
                                                , host
                                                , master
                                                )
import           Database.MongoDB.Connection    ( Host(..)
                                                , PortID
                                                )
import qualified LolHub.Domain.User            as User
import           LolHub.Graphql.Api             ( api
                                                , gqlRoot
                                                )
import           LolHub.Graphql.Types
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WebSockets
                                               as WaiWs
import           Network.Wai.Middleware.RequestLogger
import           Network.WebSockets             ( defaultConnectionOptions )

import           System.Exit
import           Web.Scotty

-- | returns the port for Scotty
portScotty = 3000
-- | returns the port of the MongoDB
portMongo :: PortID
portMongo = 37017

-- | returns the host IP for MongoDB
hostName :: String
hostName = "localhost"

getSession = do
        token <- header "Authorization"
        return
                $   User.decodeSession
                =<< ((!! 1) <$> (Text.words <$> (toStrict <$> token))) -- parse away Bearer prefix

main :: IO ()
main = do
        pipe <- connect (Host hostName portMongo)
        let settings = Warp.setPort portScotty Warp.defaultSettings
        let wsApp    = gqlSocketApp $ gqlRoot pipe Nothing
        state   <- initGQLState
        httpApp <- sapp state pipe
        Warp.runSettings settings $ WaiWs.websocketsOr
                defaultConnectionOptions
                (wsApp state)
                httpApp
        close pipe

sapp :: GQLState IO USEREVENT -> Pipe -> IO Wai.Application
sapp state pipe = scottyApp $ post "/" $ do
        session <- getSession
        raw =<< (liftIO . api pipe session =<< body)

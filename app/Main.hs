{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Core.Network.Wai.Middleware.JWT
import           Data.Maybe
import qualified Data.Text                     as Text
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Builder
import           Data.Morpheus.Types            ( RootResolver )
import           Data.Morpheus.Server           ( webSocketsApp )
import           Data.ByteString.Lazy           ( ByteString )
import           Database.MongoDB               ( Action
                                                , Document
                                                , Pipe
                                                , access
                                                , close
                                                , connect
                                                , host
                                                , master
                                                )
import           Database.MongoDB.Connection    ( readHostPort )
import qualified LolHub.Domain.User            as User
import           LolHub.Graphql.Api             ( api
                                                , subApi
                                                , gqlRoot
                                                )
import           LolHub.Graphql.Types
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Handler.Warp       ( defaultSettings
                                                , runSettings
                                                , setPort
                                                )
import           Network.Wai.Handler.WebSockets ( websocketsOr )
import           Network.WebSockets             ( ServerApp
                                                , defaultConnectionOptions
                                                )

import           System.Exit
import           Web.Scotty

-- | port for Scotty
portScotty = 3000

-- | host IP for MongoDB
hostName :: String
hostName = "localhost:27017"

getSession :: ActionM (Maybe User.SessionE)
getSession = do
        token <- header "Authorization"
        return $ User.decodeSession . User.parseAuthHeader =<< token   -- parse away Bearer prefix

main :: IO ()
main = do
        pipe <- connect (readHostPort hostName)
        scottyServer pipe
        close pipe

scottyServer :: Pipe -> IO ()
scottyServer pipe = do
        (wsApp, publish) <- webSocketsApp $ subApi pipe Nothing
        startServer wsApp $ httpEndpoint "/" api pipe

httpEndpoint
        :: RoutePattern
        -> (Pipe -> Maybe User.SessionE -> ByteString -> IO ByteString)
        -> Pipe
        -> ScottyM ()
httpEndpoint route api pipe = do
        middleware logStdoutDev -- logging
        post route $ do
                session <- getSession
                raw =<< (liftIO . api pipe session =<< body)

startServer :: ServerApp -> ScottyM () -> IO ()
startServer wsApp app = do
        httpApp <- scottyApp app
        runSettings settings
                $ websocketsOr defaultConnectionOptions wsApp httpApp
        where settings = setPort portScotty defaultSettings

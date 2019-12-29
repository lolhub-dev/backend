{-# LANGUAGE OverloadedStrings #-}

module Main where

import           LolHub.Graphql.Api (userApi, lobbyApi)
import qualified LolHub.Domain.User as User
import           Control.Monad.IO.Class
import           Web.Scotty
import           System.Exit
import           Control.Concurrent
import           Network.Wai.Middleware.RequestLogger
import           Core.Network.Wai.Middleware.JWT
import           Database.MongoDB (Action, connect, host, access, master, close
                                 , Document)
import           Database.MongoDB.Connection (Host(..), PortID)
import qualified Data.Text as Text
import           Data.Text.Lazy (toStrict)
import           Data.Maybe

-- | returns the port for Scotty
portScotty = 3000

-- | returns the port of the MongoDB
portMongo :: PortID
portMongo = 37017

-- | returns the host IP for MongoDB
hostName :: String
hostName = "127.0.0.1"

getSession = do
  token <- header "Authorization"
  return $ User.decodeSession =<< ((!!1) <$>(Text.words <$> (toStrict <$> token))) -- parse away Bearer prefix

main :: IO ()
main = do
  pipe <- connect (Host hostName portMongo)
  scotty portScotty
    $ do
      middleware logStdoutDev -- logging
      middleware
        $ jwt
          "TVwTQvknx0vaQE6mTlFJPB9VSbz5iPRS" -- JWT server secret, dont change !!! //TODO: put this in some global server env file
          ["/user", "/lobby"] -- ignored routes for authentication
      post "/user" $ raw =<< (liftIO . (userApi pipe) =<< body)
      post "/lobby"
        $ do
          session <- getSession
          raw =<< (liftIO . (lobbyApi pipe session) =<< body)
  close pipe
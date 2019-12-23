{-# LANGUAGE OverloadedStrings #-}

module Main where

import           LolHub.Connection.Graphql.UserApi (userApi)
import           Control.Monad.IO.Class
import           Web.Scotty
import           System.Exit
import           Control.Concurrent
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.JWT
import           Database.MongoDB (Action, connect, host, access, master, close
                                 , Document)
import           Database.MongoDB.Connection (Host(..), PortID)
import           LolHub.Connection.DB.Coll.User
import           LolHub.Connection.DB.Mongo (run)

-- | returns the port for Scotty
portScotty = 3000

-- | returns the port of the MongoDB
portMongo :: PortID
portMongo = 37017

-- | returns the host IP for MongoDB
hostName :: String
hostName = "127.0.0.1"

main :: IO ()
main = do
  pipe <- connect (Host hostName portMongo)
  scotty portScotty
    $ do
      middleware logStdoutDev -- logging
      middleware
        $ jwt
          "TVwTQvknx0vaQE6mTlFJPB9VSbz5iPRS" -- JWT server secret, dont change !!! //TODO: put this in some global server env file
          ["/user"] -- ignored routes for authentication
      post "/user" $ raw =<< (liftIO . (userApi pipe) =<< body)
      -- post "/gamemodes" $ raw =<< (liftIO . gamemodeApi =<< body)
  close pipe

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           LolHub.Connection.Graphql.UserApi (userApi)
import           Control.Monad.IO.Class
import           Web.Scotty
import           System.Exit
import           Control.Concurrent
import           Network.Wai.Middleware.RequestLogger
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

-- | runs the accumulated Actions 
exampleActions :: Action IO (Maybe User)
exampleActions = do
  insertRes <- insertUser $ User 1 "test" "test" "test" "test" "test" "test"
  user <- getUser "test"
  invalidUser <- getUser "invalidUser"
  return $ putStrLn $ show user
  return $ print invalidUser
  return user

main :: IO ()
main = do
  pipe <- connect (Host hostName portMongo)
  e <- run exampleActions pipe
  scotty portScotty
    $ do
      middleware logStdoutDev -- logging
<<<<<<< HEAD
      post "/api" $ raw =<< (liftIO . (userApi pipe) =<< body)
=======
      middleware
        $ jwt
          "TVwTQvknx0vaQE6mTlFJPB9VSbz5iPRS" -- JWT server secret, dont change !!! //TODO: put this in some global server env file
          ["/user"] -- ignored routes for authentication
      post "/user" $ raw =<< (liftIO . userApi =<< body)
      -- post "/gamemodes" $ raw =<< (liftIO . gamemodeApi =<< body)
>>>>>>> 5b861e8bde4b7efc6fbd50070033e011a6d2e8dc
  close pipe
  print e

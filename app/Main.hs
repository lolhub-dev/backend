{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lolhub.Connection.API
import           Control.Monad.IO.Class
import           Web.Scotty
import           System.Exit
import           System.Posix.Signals
import           Control.Concurrent
import           Network.Wai.Middleware.RequestLogger
import           Database.MongoDB (Action, connect, host, access, master, close
                                 , Document)
import           Database.MongoDB.Connection (Host(..), PortID)
import           Lolhub.Connection.DB.Coll.User
import           Lolhub.Connection.DB.Mongo (run)

-- | returns the port for Scotty
portScotty = 3000

-- | returns the port of the MongoDB
portMongo :: PortID
portMongo = 37017

-- | returns the host IP for MongoDB
hostName :: String
hostName = "127.0.0.1"

-- | runs the accumulated Actions 
exampleActions :: Action IO ()
exampleActions = do
  insertRes <- insertUser $ User "test" "test" "test" "test" "test"
  user <- getUser "test"
  print user
  invalidUser <- getUser "invalidUser";
  print invalidUser
  return ()

main :: IO ()
main = do
  pipe <- connect (Host hostName portMongo)
  e <- run exampleActions pipe
  scotty portScotty
    $ do
      middleware logStdoutDev -- logging
      post "/api" $ raw =<< (liftIO . gqlApi =<< body)
  close pipe
  print e

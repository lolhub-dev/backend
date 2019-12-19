{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lolhub.Connection.Graphql.UserApi (userApi)
import           Control.Monad.IO.Class
import           Web.Scotty

main :: IO ()
main = scotty 3000
  $ do
    post "/user" $ raw =<< (liftIO . userApi =<< body)
-- post "/gamemodes" $ raw =<< (liftIO . gamemodeApi =<< body)

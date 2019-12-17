{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Lolhub.Connection.API
import           Control.Monad.IO.Class
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)

{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Lolhub.Connection.API
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)

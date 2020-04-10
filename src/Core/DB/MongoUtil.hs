{-# LANGUAGE OverloadedStrings #-}

module Core.DB.MongoUtil
        ( run
        , encodeAction
        , parseAction
        , (<<-)
        )
where

import           Control.Concurrent.MonadIO
import           Control.Monad.Trans.Reader     ( mapReaderT )
import           Data.Bson.Mapping
import           Data.Data                      ( Typeable )
import           Database.MongoDB               ( Action
                                                , Document
                                                , Pipe
                                                , access
                                                , master
                                                )
import           Database.MongoDB.Query         ( Database )
import           GHC.Generics

-- | returns the DB name
dbName :: Database
dbName = "lolhub" -- //TODO: read from global env file

run :: MonadIO m => Action m a -> Pipe -> m a
run action pipe = access pipe master dbName action

parseAction
        :: (Bson a, MonadFail m, MonadIO io)
        => Action io (m Document)
        -> Action io (m a)
parseAction = mapReaderT maybeFromBson
    where
        maybeFromBson
                :: (Bson a, MonadFail m, MonadIO io)
                => io (m Document)
                -> io (m a)
        maybeFromBson a = (\b -> return (fromBson =<< b)) =<< a

encodeAction
        :: (Bson a, Monad m, MonadIO io)
        => Action io (m a)
        -> Action io (m Document)
encodeAction a = (\b -> return (toBson <$> b)) =<< a


(<<-)
        :: (MonadIO io)
        => (a -> Action io (Maybe b))
        -> Maybe a
        -> Action io (Maybe b)
(<<-) a b = case b of
        Just x  -> a x
        Nothing -> return Nothing


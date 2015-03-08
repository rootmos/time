{-# LANGUAGE OverloadedStrings #-}
module Serializable ( Serializable (..)
                    , Referable (..)
                    , Reference (refID)
                    , DB (..)
                    , getField
                    , getReference
                    ) where

-------------------------------------------------------------------------------

import qualified Database.MongoDB as M ( lookup
                                       , insert
                                       , find
                                       , save
                                       , findOne)
import Database.MongoDB ( Document
                        , (=:)
                        , Collection
                        , Action
                        , select
                        , Selector
                        , rest
                        , ObjectId
                        , typed
                        , merge
                        , Label
                        , Val
                        )
import Control.Monad (liftM, liftM2)
import Data.Maybe (catMaybes)

-------------------------------------------------------------------------------

class Serializable a where
    serialize :: a -> Document
    unserialize :: Document -> Maybe a

    insert :: a -> Action IO (DB a)
    insert' :: Collection -> a -> Action IO (DB a)
    insert' collection x = do
       id <- liftM typed $ M.insert collection (serialize x)
       return $ DB id x

    find :: Selector -> Action IO [DB a]
    find' :: Collection -> Selector -> Action IO [DB a]
    find' collection q = do
        docs <- rest =<< M.find (select q collection)
        return . catMaybes $ map unserializeAndWrap docs

    findOne :: Selector -> Action IO (Maybe (DB a))
    findOne' :: Collection -> Selector -> Action IO (Maybe (DB a))
    findOne' collection q = do
        doc <- M.findOne (select q collection)
        case doc of
            Nothing -> return Nothing
            Just x -> return $ unserializeAndWrap x

    save :: DB a -> Action IO ()
    save' :: Collection -> DB a -> Action IO ()
    save' collection (DB id a) =
        M.save collection $ merge ["_id" =: id] (serialize a)

unserializeAndWrap :: (Serializable a) => Document -> Maybe (DB a)
unserializeAndWrap doc = liftM2 DB (readID doc) (readObject doc)
  where
      readID = M.lookup "_id"
      readObject = unserialize

-------------------------------------------------------------------------------

getField :: (Val a) => Document -> Label -> Maybe a
getField doc name = M.lookup name doc

getReference :: Document -> Label -> Maybe (Reference a)
getReference doc name = do
    objID <- M.lookup name doc :: Maybe ObjectId
    return $ Reference objID

-------------------------------------------------------------------------------

data DB a = DB { objID :: ObjectId, get :: a }

instance Functor DB where
    fmap f (DB id a) = DB id (f a)

instance Show a => Show (DB a) where
    show (DB id x) = show x ++ " (_id: " ++ show id ++ ")"

-------------------------------------------------------------------------------

data Reference a = Reference { refID :: ObjectId }

instance Show (Reference a) where
    show x = "Reference:" ++ show (refID x)

class Referable a where
    reference :: DB a -> Reference a
    reference (DB id _) = Reference id

    dereference :: (Serializable a) => Reference a
                -> Action IO (Maybe (DB a))
    dereference' :: (Serializable a) => Collection -> Reference a
                 -> Action IO (Maybe (DB a))
    dereference' collection (Reference id) = findOne ["_id" =: id]

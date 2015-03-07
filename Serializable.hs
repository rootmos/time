{-# LANGUAGE OverloadedStrings #-}
module Serializable ( Serializable (..)
                    , DB
                    , getField
                    ) where

import qualified Database.MongoDB as M (lookup, insert, find, save)
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
        return . catMaybes $ map wrapper docs
        where
            wrapper doc = liftM2 DB (readID doc) (readObject doc)
            readID = M.lookup "_id"
            readObject = unserialize

    save :: DB a -> Action IO ()
    save' :: Collection -> DB a -> Action IO ()
    save' collecton (DB id a) =
        M.save collecton doc
          where
              doc = merge ["_id" =: id] (serialize a)

getField :: (Val a) => Document -> Label -> Maybe a
getField doc name = M.lookup name doc

data DB a = DB { id :: ObjectId, get :: a }

instance Functor DB where
    fmap f (DB id a) = DB id (f a)

instance Show a => Show (DB a) where
    show (DB id x) = show x ++ " (_id: " ++ show id ++ ")"

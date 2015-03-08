{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module TimeData ( User (..)
                , TimeRecord (..)
                , sumTimeRecords
                , Serializable.DB
                , connect
                , close
                , retrieveAllUsers
                , getUserByName
                , getUserByNameUnsafe
                , TimeData.insert
                , TimeData.save
                ) where

-------------------------------------------------------------------------------

import Network.Socket (HostName)

import Database.MongoDB ( (=:) )
import qualified Database.MongoDB as M ( connect
                                       , host
                                       , close
                                       , master
                                       , access
                                       , Pipe )

import Data.Monoid ( Monoid
                   , mconcat
                   , mempty
                   , mappend
                   )
import Data.Time ( UTCTime
                 , NominalDiffTime
                 , diffUTCTime
                 )
import Control.Applicative ( (<$>) )
import Data.Maybe (catMaybes, listToMaybe, fromJust)
import Control.Monad ( liftM )

import FixedPointData
import Serializable

-------------------------------------------------------------------------------

data User = User { name :: String
                 , targetHours :: Float
                 }
                 deriving (Show)

instance Serializable User where
    serialize user = [ "name" =: (name user)
                     , "targetHours" =: (targetHours user)
                     ]
    unserialize doc = do
       name <- getField doc "name" :: Maybe String
       hours <- getField doc "targetHours" :: Maybe Float
       return (User name hours)

    insert = insert' "users"
    find = find' "users"
    findOne = findOne' "users"
    save = save' "users"

instance Referable User where
    dereference = dereference' "users"

-------------------------------------------------------------------------------

type Timestamp = UTCTime
type When = Timestamp
type Amount = NominalDiffTime

instance Monoid NominalDiffTime where
    mempty = 0 :: NominalDiffTime
    mappend = (+)

data TimeRecord = Set (Reference User) When Amount
                | Add (Reference User) When Amount
                | Between (Reference User) Timestamp Timestamp
                deriving (Show)

getValue :: TimeRecord -> Amount
getValue (Set _ _ x) = x
getValue (Add _ _ x) = x
getValue (Between _ x y) = diffUTCTime y x

getWhen :: TimeRecord -> When
getWhen (Set _ x _) = x
getWhen (Add _ x _) = x
getWhen (Between _ x _) = x

sumTimeRecords :: [TimeRecord] -> Amount
sumTimeRecords = getData . mconcat . map getFixedPointData
    where
        getFixedPointData (Set _ _ x) = Fixed x
        getFixedPointData x = Data (getValue x)

instance Serializable TimeRecord where
    serialize (Set u w a) = [ "when" =: w
                            , "set" =: a
                            , "user" =: refID u
                            ]
    serialize (Add u w a) = [ "when" =: w
                            , "add" =: a
                            , "user" =: refID u
                            ]
    serialize (Between u s e) = [ "start" =: s
                                , "end" =: e
                                , "user" =: refID u
                                ]

    unserialize doc = listToMaybe . catMaybes $ [ tryParseSet doc
                                                , tryParseAdd doc
                                                , tryParseBetween doc
                                                ]
        where
            tryParseSet doc = do
                amount <- getField doc "set"
                when <- getField doc "when"
                user <- getReference doc "user"
                return $ Set user when amount
            tryParseAdd doc = do
                amount <- getField doc "add"
                when <- getField doc "when"
                user <- getReference doc "user"
                return $ Add user when amount
            tryParseBetween doc = do
                start <- getField doc "start"
                end <- getField doc "end"
                user <- getReference doc "user"
                return $ Between user start end

    insert = insert' "records"
    find = find' "records"
    findOne = findOne' "records"
    save = save' "records"

-------------------------------------------------------------------------------

type Connection = M.Pipe

connect :: HostName -> IO Connection
connect = M.connect . M.host

close :: Connection -> IO ()
close = M.close

run con = M.access con M.master "time"

-------------------------------------------------------------------------------

retrieveAllUsers :: Connection -> IO [DB User]
retrieveAllUsers con = run con $ find []

getUserByName :: Connection -> String -> IO (Maybe (DB User))
getUserByName con name = liftM listToMaybe $ run con $ find ["name" =: name]

getUserByNameUnsafe :: Connection -> String -> IO (DB User)
getUserByNameUnsafe con name = liftM fromJust $ getUserByName con name

insert :: (Serializable a) => Connection -> a -> IO (DB a)
insert con = run con . Serializable.insert

save :: (Serializable a) => Connection -> DB a -> IO ()
save con = run con . Serializable.save

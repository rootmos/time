{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Database.MongoDB ( (=:)
                        , connect
                        , access
                        , master
                        , host
                        , close
                        )
import Data.Monoid ( Monoid
                   , mconcat
                   , mempty
                   , mappend
                   )
import Data.Time ( UTCTime
                 , NominalDiffTime
                 , diffUTCTime
                 , addUTCTime
                 , getCurrentTime
                 )
import Control.Applicative ( (<$>) )
import Control.Monad ( liftM )
import Data.Maybe (catMaybes, listToMaybe)

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

sumTimeRecords :: [TimeRecord] -> NominalDiffTime
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

main :: IO ()
main = do
    pipe <- connect (host "127.0.0.1")

    -- User testing -----------------------------------------------------------
    --insertUser pipe "lars"
    user <- getUnsafe pipe "lars" :: IO (DB User)
    --run pipe . save $ (\u -> u { targetHours = (targetHours u) + 1.0 }) <$> user
    --printUsers pipe

    -- TimeRecord testing -----------------------------------------------------
    --insertRecords pipe user
    printTimeRecords pipe
    close pipe

insertUser pipe name = run pipe $ insert (User name 40.0)
getUnsafe pipe name = liftM head $ run pipe $ find ["name" =: name]
printUsers pipe = do
    users <- run pipe $ find [] :: IO [DB User]
    mapM (putStrLn . show) users

insertRecords pipe user = do
    now <- getCurrentTime
    s <- run pipe . insert $ Set (reference user) now 10
    putStrLn $ show s

    now <- getCurrentTime
    a <- run pipe . insert $ Add (reference user) now 7
    putStrLn $ show a

    now <- getCurrentTime
    b <- run pipe . insert $ Between (reference user) now (addUTCTime 20 now)
    putStrLn $ show b

printTimeRecords pipe = do
    records <- run pipe $ find [] :: IO [DB TimeRecord]
    mapM (putStrLn . show) records

run pipe = access pipe master "time"

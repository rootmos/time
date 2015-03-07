{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Database.MongoDB ( (=:)
                        , connect
                        , access
                        , master
                        , host
                        , close
                        )
import Data.Monoid
import Data.Time
import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans

import FixedPointData
import Serializable

data User = User { name :: String
                 , targetHours :: Float
                 }
                 deriving (Show)

type Timestamp = UTCTime
type When = Timestamp
type Amount = NominalDiffTime

instance Monoid NominalDiffTime where
    mempty = 0 :: NominalDiffTime
    mappend = (+)

data TimeRecord = Set When Amount
                | Add When Amount
                | Between Timestamp Timestamp
                deriving (Show)

getValue :: TimeRecord -> Amount
getValue (Set _ x) = x
getValue (Add _ x) = x
getValue (Between x y) = diffUTCTime y x

getWhen :: TimeRecord -> When
getWhen (Set x _) = x
getWhen (Add x _) = x
getWhen (Between x _) = x

sumTimeRecords :: [TimeRecord] -> NominalDiffTime
sumTimeRecords = getData . mconcat . map getFixedPointData
    where
        getFixedPointData (Set _ x) = Fixed x
        getFixedPointData x = Data (getValue x)

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
    save = save' "users"


main :: IO ()
main = do
    pipe <- connect (host "127.0.0.1")
    --insertUser pipe "lars"
    user <- getUnsafe pipe "lars" :: IO (DB User)
    run pipe . save $ (\u -> u { targetHours = (targetHours u) + 1.0 }) <$> user
    printUsers pipe
    close pipe

insertUser pipe name = run pipe $ insert (User name 40.0)

getUnsafe pipe name = liftM head $ run pipe $ find ["name" =: name]

printUsers pipe = do
    users <- run pipe $ find [] :: IO [DB User]
    mapM (putStrLn . show) users

run pipe = access pipe master "time"

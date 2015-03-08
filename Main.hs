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
    save = save' "users"

-------------------------------------------------------------------------------

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

instance Serializable TimeRecord where
    serialize (Set w a) = [ "when" =: w, "set" =: a ]
    serialize (Add w a) = [ "when" =: w, "add" =: a ]
    serialize (Between s e) = [ "start" =: s, "end" =: e ]

    unserialize doc = listToMaybe . catMaybes $ [ tryParseSet doc
                                                , tryParseAdd doc
                                                , tryParseBetween doc
                                                ]
        where
            tryParseSet doc = do
                amount <- getField doc "set"
                when <- getField doc "when"
                return $ Set when amount
            tryParseAdd doc = do
                amount <- getField doc "add"
                when <- getField doc "when"
                return $ Add when amount
            tryParseBetween doc = do
                start <- getField doc "start"
                end <- getField doc "end"
                return $ Between start end



    insert = insert' "records"
    find = find' "records"
    save = save' "records"

-------------------------------------------------------------------------------

main :: IO ()
main = do
    pipe <- connect (host "127.0.0.1")

    -- User testing -----------------------------------------------------------
    --insertUser pipe "lars"
    --user <- getUnsafe pipe "lars" :: IO (DB User)
    --run pipe . save $ (\u -> u { targetHours = (targetHours u) + 1.0 }) <$> user
    --printUsers pipe

    -- TimeRecord testing -----------------------------------------------------
    --insertRecords pipe
    printTimeRecords pipe
    close pipe

insertUser pipe name = run pipe $ insert (User name 40.0)
getUnsafe pipe name = liftM head $ run pipe $ find ["name" =: name]
printUsers pipe = do
    users <- run pipe $ find [] :: IO [DB User]
    mapM (putStrLn . show) users

insertRecords pipe = do
    now <- getCurrentTime
    s <- run pipe . insert $ Set now 10
    putStrLn $ show s

    now <- getCurrentTime
    a <- run pipe . insert $ Add now 7
    putStrLn $ show a

    now <- getCurrentTime
    b <- run pipe . insert $ Between now (addUTCTime 20 now)
    putStrLn $ show b

printTimeRecords pipe = do
    records <- run pipe $ find [] :: IO [DB TimeRecord]
    mapM (putStrLn . show) records

run pipe = access pipe master "time"

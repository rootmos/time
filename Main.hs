{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Data.Time ( addUTCTime
                 , getCurrentTime
                 )

import Database.MongoDB ( (=:)
                        , connect
                        , access
                        , master
                        , host
                        , close
                        )

import Control.Monad ( liftM )

import TimeData

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

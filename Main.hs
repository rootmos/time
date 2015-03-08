{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Data.Time
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

import TimeData

-------------------------------------------------------------------------------

main :: IO ()
main = do
    tz <- getCurrentTimeZone
    con <- connect "127.0.0.1"

    -- insert con (User "Foobar" 7.75)
    let from = localTimeToUTC tz $ readTime defaultTimeLocale "%F" "2015-03-01"
    now <- getCurrentTime

    foobar <- getUserByNameUnsafe con "lars"

    records <- retrieveRecordsForUser con foobar from now
    mapM (putStrLn . show) records

    -- users <- retrieveAllUsers con
    -- mapM (putStrLn . show) users

    close con

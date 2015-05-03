module ParseTime (niceParseTime) where

import Control.Monad
import Data.Char
import Data.Time
import Data.List
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Calendar.WeekDate
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

match with input = and (zipWith (==) (map toLower with) (map toLower input))

today :: String -> IO (Maybe UTCTime)
today input = if match "today" input then todaysUTCTime >>= return . Just
                          else return Nothing
              where
                  todaysUTCTime = do
                      now <- getCurrentTime
                      return $ UTCTime (utctDay now) (secondsToDiffTime 0)

yesterday :: String -> IO (Maybe UTCTime)
yesterday input = if match "yesterday" input  then yesterdaysUTCTime >>= return . Just
                          else return Nothing
              where
                  yesterdaysUTCTime = do
                      now <- getCurrentTime
                      return $ UTCTime (addDays (-1) $ utctDay now) (secondsToDiffTime 0)

parseWeekdays input = do
    weekdays <- getWeekdays
    case find (\(_, name) -> match name input) weekdays of
      Just (day, _) -> return . Just $ UTCTime day (secondsToDiffTime 0)
      Nothing -> return Nothing
      where
          getStartOfWeek = do
              now <- getCurrentTime
              let (year, week, _) = toWeekDate (utctDay now)
              return $ fromWeekDate year week 0
          getWeekdays = do
             startOfWeek <- getStartOfWeek
             return $ map ($ startOfWeek) [monday, tuesday, wednesday, thursday, friday, saturday, sunday]
          monday startOfWeek = (startOfWeek, "Monday")
          tuesday startOfWeek = (addDays 1 startOfWeek, "Tuesday")
          wednesday startOfWeek = (addDays 2 startOfWeek, "Wednesday")
          thursday startOfWeek = (addDays 3 startOfWeek, "Thursday")
          friday startOfWeek = (addDays 4 startOfWeek, "Friday")
          saturday startOfWeek = (addDays 5 startOfWeek, "Saturday")
          sunday startOfWeek = (addDays 6 startOfWeek, "Sunday")

parseRFC822 = parseTimeM True defaultTimeLocale rfc822DateFormat

parseISO8601 input = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) input
       `mplus` parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M") input
       `mplus` parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") input
       `mplus` parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H%M") input
       `mplus` parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H%M%S") input

parsers :: [String -> IO (Maybe UTCTime)]
parsers = [ yesterday
          , today
          , parseWeekdays
          , return . parseISO8601
          , return . parseRFC822
          ]

niceParseTime :: String -> IO (Maybe UTCTime)
niceParseTime input = liftM msum . sequence $ map ($ input) parsers

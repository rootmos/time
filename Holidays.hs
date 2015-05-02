module Holidays ( DayContext
                , getDay
                , getType
                , describe
                , DayType (..)
                , getContext
                , isWorkday
                ) where

import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.Easter
import Data.Time.Format
import Control.Monad

-- DayContext
data DayContext = DayContext { getDay :: Day
                             , getType :: DayType
                             , describe :: String
                             }
                             deriving (Show)

data DayType = Workday | Halfday | Off
    deriving (Show)

-- Our list of engines
holidayEngines = [swedishHolidaysEngine, christianHolidaysEngine, weekdayEngine]

getContext :: Day -> DayContext
getContext day = fromJust . msum $ map ($ day) holidayEngines

isWorkday :: DayContext -> Bool
isWorkday context = case getType context of
                  Workday -> True
                  Halfday -> True
                  _ -> False

-- The weekday engine
weekdayEngine :: Day -> Maybe DayContext
weekdayEngine day
        | dayOfWeek day >= 6 = Just $ DayContext day Off (weekdayName day)
        | otherwise = Just $ DayContext day Workday (weekdayName day)
          where
              weekdayName = formatTime defaultTimeLocale "%A"

-- Helpers
first (x, _, _) = x
second (_, x, _) = x
third (_, _, x) = x

dayOfWeek = third . toWeekDate
dayOfMonth = third . toGregorian 
month = second . toGregorian
year = first . toGregorian

-- Christian Holidays
maundyThursday year = DayContext (addDays (-1) . getDay $ goodFriday year) Halfday "Maundy Thursday"
goodFriday year = DayContext (addDays (-2) . getDay $ easterDay year) Off "Good Friday"
easterDay year = DayContext (gregorianEaster year) Off "Easter Day"
easterMonday year = DayContext (addDays 1 . getDay $ easterDay year) Off "Easter Monday"
ascentionDay year = DayContext (addDays 39 . getDay $ easterDay year) Off "Ascention Day"
whitsunday year = DayContext (addDays (7*7) . getDay $ easterDay year) Off "Whitsunday"
firstAdvent year = DayContext (addDays (-7) . getDay $ secondAdvent year) Off "1st Advent"
secondAdvent year = DayContext (addDays (-7) . getDay $ thirdAdvent year) Off "2nd Advent"
thirdAdvent year = DayContext (addDays (-7) . getDay $ forthAdvent year) Off "3rd Advent"
forthAdvent year = DayContext (previousSunday . getDay $ christmasDay year) Off "4th Advent"
  where 
    previousSunday day = head $ dropWhile (\day -> 7 /= dayOfWeek day) [pred day,(pred . pred) day..]
christmasEve year = DayContext (fromGregorian year 12 24) Off "Christmas Eve"
christmasDay year = DayContext (fromGregorian year 12 25) Off "Christmas Day"
boxingDay year = DayContext (fromGregorian year 12 26) Off "Boxing Day"
newYearsEve year = DayContext (fromGregorian year 12 31) Off "New Years Eve"
newYearsDay year = DayContext (fromGregorian year 1 1) Off "New Years Day"
epiphany year = DayContext (addDays 13 . getDay $ christmasEve (year - 1)) Off "Epiphany"

christianHolidays = [ maundyThursday
                    , goodFriday
                    , easterDay
                    , easterMonday
                    , ascentionDay
                    , whitsunday
                    , firstAdvent
                    , secondAdvent
                    , thirdAdvent
                    , forthAdvent
                    , christmasEve
                    , christmasDay
                    , boxingDay
                    , newYearsEve
                    , newYearsDay
                    , epiphany
                    ]

christianHolidaysEngine :: Day -> Maybe DayContext
christianHolidaysEngine day = find (\holiday -> getDay holiday == day) $ map ($ year day) christianHolidays

-- Swedish Holidays
walpurgisNight year = DayContext (fromGregorian year 4 30) Halfday "Walpurgis Night"
labourDay year = DayContext (fromGregorian year 5 1) Off "Labour Day"
swedishNationalDay year = DayContext (fromGregorian year 6 6) Off "Swedish National Day"
midsummerEve year = DayContext (addDays (-1) . getDay $ midsummerDay year) Off "Midsummer Eve"
midsummerDay year = DayContext (head $ dropWhile (\day -> 6 /= dayOfWeek day) [(fromGregorian year 6 20)..(fromGregorian year 6 26)])
                        Off "Midsummer Day"
allSaintsEve year = DayContext (addDays (-1) . getDay $ allSaintsDay year) Halfday "All Saints' Eve"
allSaintsDay year = DayContext (head $ dropWhile (\day -> 6 /= dayOfWeek day) [(fromGregorian year 10 31)..(fromGregorian year 11 6)])
                        Off "All Saints' Day"

swedishHolidays = [ walpurgisNight
                  , labourDay
                  , swedishNationalDay
                  , midsummerEve
                  , midsummerDay
                  , allSaintsEve
                  , allSaintsDay
                  ]

swedishHolidaysEngine :: Day -> Maybe DayContext
swedishHolidaysEngine day = find (\holiday -> day == getDay holiday) $ map ($ year day) swedishHolidays

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Time
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Data.Time.Format
import Control.Exception
import Control.Monad

import System.IO
import System.Console.Readline
import Options.Applicative
import Text.Printf

-- Our stuff
import TimeData
import TimeConfiguration
import Shlex
import Ask
import ParseTime
import ParseDurations
import Holidays

exitCmds = ["quit", "exit"]

readlineAndDoTheThing con user = do
    maybe <- readline "> "
    case maybe of
      Nothing -> return ()
      Just x | x `elem` exitCmds -> return ()
      Just x | otherwise -> do
         addHistory x
         either putStrLn (delegate con user) (parseCmdLine x)
         readlineAndDoTheThing con user


parseCmdLine :: String -> Either String CommandOptions
parseCmdLine input = do
    argv <- shlex input
    case execParserPure parserOptions cmdline argv of
      Success x -> Right x
      Failure e -> Left . fst $ renderFailure e []
    where
        cmdline = info (helper <*> commandParser) (
            fullDesc
            <> progDesc "Time management program"
            <> header "time - a program for managing time accounts" )
        parserOptions = ParserPrefs "foo" False False True 80

delegate con user opt = do
    result <- try (execute con user opt) :: IO (Either SomeException ())
    case result of
      Left e -> print e
      Right _ -> return ()

execute con user (AddOptions amount maybeOn) = do
    time <- case maybeOn of
            Just x -> do
                parsed <- niceParseTime x
                case parsed of
                  Just y -> return y
                  Nothing -> error $ "Unable to parse --after=" ++ x
            Nothing -> getCurrentTime
    let seconds = realToFrac . secondsToDiffTime $ parseDurationsUnsafe amount
    record <- insert con (Add (reference user) time seconds)
    putStrLn . show $ record
execute con user (ShowOptions maybeAfter maybeBefore) = do
    now <- getCurrentTime
    after <- case maybeAfter of
               Just x -> do
                   parsed <- niceParseTime x
                   case parsed of
                     Just y -> return y
                     Nothing -> error $ "Unable to parse --after=" ++ x
               Nothing -> return $ startOfDay now
    before <- case maybeBefore of
               Just x -> do
                   parsed <- niceParseTime x
                   case parsed of
                     Just y -> return y
                     Nothing -> error $ "Unable to parse --before=" ++ x
               Nothing -> return $ endOfDay now
    showRecords con user after before
execute con user (WeekOptions maybeWhen) = do
    time <- case maybeWhen of
              Nothing -> getCurrentTime
              Just input -> do
                  maybeTime <- niceParseTime input
                  case maybeTime of
                    Just x -> return x
                    Nothing -> do
                        now <- getCurrentTime
                        case parseDurations input of
                          Right x -> do
                              return $ addUTCTime (fromIntegral x :: NominalDiffTime) now
                          Left _ -> error $ "Unable to parse " ++ input
    showRecords con user (startOfWeek time) (endOfWeek time)

startOfDay time =
    let (year, month, day) = toGregorian . utctDay $ time in
    UTCTime (fromGregorian year month day) (secondsToDiffTime 0)

endOfDay time =
    let (year, month, day) = toGregorian . utctDay $ time in
    addUTCTime ((-1) :: NominalDiffTime) $ UTCTime (fromGregorian year month (day+1)) (secondsToDiffTime 0)

startOfWeek time =
    let (year, week, _) = toWeekDate . utctDay $ time in
    UTCTime (fromWeekDate year week 0) (secondsToDiffTime 0)

endOfWeek time =
    let (year, week, _) = toWeekDate . utctDay $ time in
    addUTCTime ((-1) :: NominalDiffTime) $ UTCTime (fromWeekDate year (week+1) 0) (secondsToDiffTime 0)

showRecords con user after before = do
    records <- liftM (map get) $ retrieveRecordsForUser con user after before
    let sorted = sortRecordsByDay records
    let days = [(utctDay after)..(utctDay before)]
    forM_ days (\day -> showDay (targetHours . get $ user) day (maybe [] id $ lookup day sorted))

showDay targetHours day rs =
    printf "%s %.2fh %s(%s,%.2fh)\n"
        (formatTime defaultTimeLocale "%F" $ day)
        (getHours . sumTimeRecords $ rs)
        (describe . getContext $ day)
        (show . getType . getContext $ day)
        (convertTargetHours . getType . getContext $ day)
          where
              convertTargetHours Workday = targetHours
              convertTargetHours Halfday = targetHours
              convertTargetHours Off = 0.0

data CommandOptions = AddOptions { amount :: String, when :: Maybe String}
                    | ShowOptions { after :: Maybe String, before :: Maybe String }
                    | WeekOptions { when :: Maybe String }
                    deriving Show

addCommandOptions :: Parser CommandOptions
addCommandOptions = AddOptions
    <$> strArgument (metavar "AMOUNT")
    <*> optional ( strOption
                 ( long "on"
                 <> short 'o'
                 <> metavar "ON"
                 ) )

showCommandOptions :: Parser CommandOptions
showCommandOptions = ShowOptions
    <$> optional ( strOption 
                 ( long "after"
                  <> short 'a'
                  <> metavar "AFTER"
                  ) )
    <*> optional ( strOption
                 ( long "before"
                 <> short 'b'
                 <> metavar "BEFORE"
                 ) )

weekCommandOptions :: Parser CommandOptions
weekCommandOptions = WeekOptions <$> optional ( strArgument (metavar "WHEN") ) 

commandParser = subparser
    ( command "add" ( info (helper <*> addCommandOptions)
                    ( progDesc "Add a time record" ) )
   <> command "show" ( info (helper <*> showCommandOptions)
                     ( progDesc "Show the time records between dates" ) )
   <> command "week" ( info (helper <*> weekCommandOptions)
                     ( progDesc "Show the time records for a week" ) )
    )

obtainUser con username = do
    maybeUser <- getUserByName con username
    case maybeUser of
      Nothing -> perhapsAddUser con username
      Just user -> return user

perhapsAddUser con username = do
    answer <- askNicely $ "Should I add the user " ++ username ++ "?"
    if answer then addUser con username
              else error $ "User not found: " ++ username

addUser con username = do
    putStr "Target hours? "
    hFlush stdout
    targetHours <- liftM read getLine :: IO Float
    insert con (User username targetHours)

main :: IO ()
main = do
    conf <- getConfiguration
    putStrLn . show $ conf
    bracket (aquire conf) release (\con -> do
            user <- obtainUser con (username conf)
            readlineAndDoTheThing con user)
      where
          aquire conf = connect (hostname conf) (fromIntegral . port $ conf)
          release = close


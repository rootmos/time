{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Time
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Data.Time.Format
import System.Locale
import Control.Exception
import Control.Monad

import System.Console.Readline
import Options.Applicative

-- Our stuff
import TimeData
import TimeConfiguration
import Shlex
import Ask
import ParseTime
import ParseDurations

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

execute con user (AddOptions amount) = do
    time <- getCurrentTime
    let seconds = realToFrac . secondsToDiffTime $ parseDurationsUnsafe amount
    record <- insert con (Add (reference user) time seconds)
    putStrLn . show $ record
execute con user (ShowOptions maybeAfter maybeBefore) = do
    after <- case maybeAfter of
               Just x -> do
                   parsed <- niceParseTime x
                   case parsed of
                     Just y -> return y
                     Nothing -> error $ "Unable to parse --after=" ++ x
               Nothing -> startOfWeek
    before <- case maybeBefore of
               Just x -> do
                   parsed <- niceParseTime x
                   case parsed of
                     Just y -> return y
                     Nothing -> error $ "Unable to parse --before=" ++ x
               Nothing -> getCurrentTime
    records <- retrieveRecordsForUser con user after before
    putStrLn . show $ map sumTimeRecords (partitionRecordsByDay (map get records))
      where
          startOfWeek = liftM (toWeekDate . utctDay) getCurrentTime >>=
              \(year, week, _) -> return $ UTCTime (fromWeekDate year week 0) (secondsToDiffTime 0)


data CommandOptions = AddOptions { amount :: String }
                    | ShowOptions { after :: Maybe String, before :: Maybe String }
                    deriving Show

addCommandOptions :: Parser CommandOptions
addCommandOptions = AddOptions <$> strArgument (metavar "AMOUNT")

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

commandParser = subparser
    ( command "add" ( info (helper <*> addCommandOptions)
                    ( progDesc "Add a time record" ) )
   <> command "show" ( info (helper <*> showCommandOptions)
                     ( progDesc "Show the time records" ) )
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

addUser con username = insert con (User username 40.0)

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


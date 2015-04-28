{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Time
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale
import Control.Exception

import System.Console.Readline
import Options.Applicative

-- Our stuff
import TimeData
import TimeConfiguration
import Shlex
import Ask

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

delegate _ _ (AddOptions _) = putStrLn "adding stuff"
delegate _ _ (ShowOptions _ _) = putStrLn "showing stuff"

data CommandOptions = AddOptions { amount :: Int }
                    | ShowOptions { after :: Maybe Int, before :: Maybe Int }
                    deriving Show

addCommandOptions :: Parser CommandOptions
addCommandOptions = AddOptions <$> argument auto (metavar "AMOUNT")

showCommandOptions :: Parser CommandOptions
showCommandOptions = ShowOptions
    <$> optional ( option auto
                 ( long "after"
                 <> short 'a'
                 <> metavar "AFTER"
                 ) )
    <*> optional ( option auto
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


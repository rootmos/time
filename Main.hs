{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Time
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

import TimeData
import TimeConfiguration

import System.Console.Readline

import Shlex
import Options.Applicative

exitCmds = ["quit", "exit"]

readlineAndDoTheThing = do
    maybe <- readline "> "
    case maybe of
      Nothing -> return ()
      Just x | x `elem` exitCmds -> return ()
      Just x | otherwise -> do
         addHistory x
         either putStrLn delegate (parseCmdLine x)  
         readlineAndDoTheThing


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

delegate :: CommandOptions -> IO ()
delegate (AddOptions _) = putStrLn "adding stuff"
delegate (ShowOptions _ _) = putStrLn "showing stuff"

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

main :: IO ()
main = do
    conf <- getConfiguration
    putStrLn . show $ conf
    readlineAndDoTheThing

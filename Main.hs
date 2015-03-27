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
         delegate x
         readlineAndDoTheThing


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

parserOptions = ParserPrefs "foo" False False True 80

delegate input = putStrLn $ case shlex input of
             Left e -> show e
             Right x -> show $ execParserPure parserOptions cmdline x
    where
        cmdline = info (helper <*> commandParser) (
            fullDesc
            <> progDesc "Time management program"
            <> header "time - a program for managing time accounts" )

main :: IO ()
main = do
    conf <- getConfiguration
    putStrLn . show $ conf
    readlineAndDoTheThing

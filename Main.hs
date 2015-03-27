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

exitCmds = ["quit", "exit"]

readlineAndDoTheThing = do
    maybe <- readline "> "
    case maybe of
      Nothing -> return ()
      Just x | x `elem` exitCmds -> return ()
      Just x | otherwise -> delegate x >> readlineAndDoTheThing

delegate input = putStrLn $ case shlex input of
                              Left e -> show e
                              Right x -> show x

main :: IO ()
main = do
    conf <- getConfiguration
    putStrLn . show $ conf
    readlineAndDoTheThing

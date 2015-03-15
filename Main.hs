{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Time
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

import TimeData
import TimeConfiguration

main :: IO ()
main = do
    conf <- getConfiguration
    putStrLn . show $ conf

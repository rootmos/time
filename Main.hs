{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Time
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale
import Text.Read (readMaybe)
import Control.Monad
import Control.Exception
import System.FilePath
import System.Directory

import Data.ConfigFile

import TimeData
import Options.Applicative
import Maybeify

-------------------------------------------------------------------------------

data Configuration = Configuration { hostname :: String
                                   , port :: Int
                                   , username :: String
                                   }
    deriving Show

maybeify ''Configuration [''Show]

emptyConfiguration :: MaybeConfiguration
emptyConfiguration = MaybeConfiguration Nothing Nothing Nothing

handleIOException :: IOException -> IO MaybeConfiguration
handleIOException _ = return emptyConfiguration

-------------------------------------------------------------------------------

configurationArgumentParser :: Parser MaybeConfiguration
configurationArgumentParser = MaybeConfiguration
    <$> optional ( strOption
                 ( long "host"
                 <> short 'H'
                 <> metavar "HOSTNAME"
                 <> help "Hostname for the MongoDB server") )
    <*> optional ( option auto
                 ( long "port"
                 <> short 'p'
                 <> metavar "PORT"
                 <> help "Port to use for contacting the MongoDB server") )
    <*> optional ( strOption
                 ( long "name"
                 <> short 'n'
                 <> metavar "NAME"
                 <> help "Specify the active user") )

parseArguments :: IO MaybeConfiguration
parseArguments = execParser opts
    where
        opts = info (helper <*> configurationArgumentParser) (
            fullDesc
            <> progDesc "Time management program"
            <> header "time - a program for managing time accounts" )

-------------------------------------------------------------------------------

parseConfigurationFile :: FilePath -> IO MaybeConfiguration
parseConfigurationFile path = handle handleIOException $
    either (\_ -> emptyConfiguration) parseHelper `liftM` readfile emptyCP path
      where 
          parseHelper parser = MaybeConfiguration (strOption "hostname")
                                                  (readOption "port")
                                                  (strOption "username")
            where
                strOption = either (\_ -> Nothing) Just . get parser "server"
                readOption = either (\_ -> Nothing) readMaybe . get parser "server"

-------------------------------------------------------------------------------

mergeField :: String -> [Maybe a] -> Either String a
mergeField field = maybe (Left ("no " ++ field)) Right . msum

mergeOptions :: [MaybeConfiguration] -> Either String Configuration
mergeOptions configs = do
    host <- mergeField "hostname" (map maybeHostname configs)
    port <- mergeField "port" (map maybePort configs)
    username <- mergeField "username" (map maybeUsername configs)
    return $ Configuration host port username

-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- parseArguments

    appPath <- getAppUserDataDirectory "time-tracker"
    conf <- parseConfigurationFile (appPath </> "config")
    last <- parseConfigurationFile (appPath </> "last")

    putStrLn . show $ mergeOptions [args, conf, last]

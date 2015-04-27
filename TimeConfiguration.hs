{-# LANGUAGE TemplateHaskell #-}
module TimeConfiguration ( Configuration
                         , getConfiguration
                         , hostname
                         , port
                         , username
                         ) where

import Text.Read (readMaybe)
import Data.ConfigFile ( readfile
                       , get
                       , set
                       , emptyCP
                       , to_string
                       , add_section
                       , ConfigParser
                       , CPError
                       )
import Control.Monad ( liftM
                     , msum
                     )
import Control.Exception ( IOException
                         , handle
                         )
import System.FilePath ( dropFileName
                       , (</>) )
import System.Directory ( getAppUserDataDirectory
                        , createDirectoryIfMissing
                        )
import Options.Applicative
import System.Exit (exitFailure)
import System.Environment (getProgName)
import Data.Either.Utils (forceEither)

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

createConfigParser :: Configuration -> Either CPError ConfigParser
createConfigParser config =
    add_section emptyCP "server"
    >>= insertStr "hostname" (hostname config)
    >>= insertShow "port" (port config)
    >>= insertStr "username" (username config)
      where
          insertStr field value = (\parser -> set parser "server" field value)
          insertShow field value = (\parser -> set parser "server" field (show value))


saveConfigurationFile :: Configuration -> FilePath -> IO ()
saveConfigurationFile config path = do
    createDirectoryIfMissing True . dropFileName $ path
    writeFile path . to_string . forceEither . createConfigParser $ config

-------------------------------------------------------------------------------

missingField :: String -> String
missingField field = "Not able to determine " ++ field

mergeField :: String -> [Maybe a] -> Either String a
mergeField field = maybe (Left (missingField field)) Right . msum

mergeOptions :: [MaybeConfiguration] -> Either String Configuration
mergeOptions configs = do
    host <- mergeField "hostname" (map maybeHostname configs)
    port <- mergeField "port" (map maybePort configs)
    username <- mergeField "username" (map maybeUsername configs)
    return $ Configuration host port username

-------------------------------------------------------------------------------

configurationPath :: IO FilePath
configurationPath = getProgName >>= getAppUserDataDirectory

mainConfig :: IO FilePath
mainConfig = flip liftM configurationPath (</> "config")

lastConfig :: IO FilePath
lastConfig = flip liftM configurationPath (</> "last")

-------------------------------------------------------------------------------

getConfiguration :: IO Configuration
getConfiguration = do
    args <- parseArguments
    conf <- parseConfigurationFile =<< mainConfig
    last <- parseConfigurationFile =<< lastConfig

    case mergeOptions [args, conf, last] of
      Left msg -> error msg
      Right opts -> (saveConfigurationFile opts =<< lastConfig) >> return opts

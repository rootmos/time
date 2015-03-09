{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Time
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

import TimeData
import Options.Applicative
import Maybeify

-------------------------------------------------------------------------------

data Configuration = Configuration { hostname :: String
                                   , port :: Int
                                   , userName :: String
                                   }
    deriving Show

maybeify ''Configuration [''Show]

sample :: Parser MaybeConfiguration
sample = MaybeConfiguration
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

main :: IO ()
main = do
    opt <- execParser opts
    putStrLn $ show opt
      where
          opts = info (helper <*> sample) (
              fullDesc
              <> progDesc "Time management program"
              <> header "time - a program for managing time accounts" )

module ParseDurations (parseDurations, parseDurationsUnsafe) where

import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (haskellDef)

number = do
    factor <- option 1 (oneOf "-" >> return (-1))
    parsecMap (multiplier factor) $ naturalOrFloat (makeTokenParser haskellDef)

multiplier factor (Left x) = Left (factor * x)
multiplier factor (Right x) = Right ((fromInteger factor) * x)

summer (Left x) (Left y) = Left (x + y)
summer (Right x) (Right y) = Right (x + y)
summer (Left x) (Right y) = Right (fromInteger x + y)
summer (Right x) (Left y) = Right (x + fromInteger y)

rounder (Left x) = x
rounder (Right x) = round x

unit = parsecMap unitToSeconds $ oneOf "wWdDhHmMsS"
  where
      unitToSeconds x
        | toLower x == 's' = 1
        | toLower x == 'm' = 60
        | toLower x == 'h' = 60*60
        | toLower x == 'd' = 60*60*24
        | toLower x == 'w' = 60*60*24*7

duration = do
    n <- number
    u <- unit
    return $ multiplier u n

durations = many duration

parseDurations input = case parse durations "durations" input of
                        Right xs -> Right . rounder $ foldr summer (Left 0) xs
                        Left e -> Left e

parseDurationsUnsafe input = case parseDurations input of 
                        Right x -> x
                        Left e -> error (show e)

main = do
    putStrLn . show $ parseDurations "20m"

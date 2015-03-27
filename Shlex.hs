{-# LANGUAGE FlexibleContexts #-}
module Shlex (shlex) where

import Text.ParserCombinators.Parsec
import Control.Monad.Error.Class

line = sepBy word spaces

word = do
    quotedWord <|> many1 (noneOf "\" ")

quotedWord = do
    char '"'
    content <- many quotedChar
    char '"'
    return content

quotedChar = do
    try (string "\\\"" >> return '"')
    <|> noneOf "\""

shlex :: MonadError String m => String -> m [String]
shlex input = case parse line "cmdline" input of
                Left e -> throwError $ show e
                Right x -> return x

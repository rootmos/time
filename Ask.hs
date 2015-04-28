module Ask (askNicely, confirm) where

import System.IO ( hFlush
                 , stdout
                 )
import Control.Monad (liftM)
import Data.Char (toLower)

askNicely :: String -> IO Bool
askNicely = askTheQuestion False 

confirm :: String -> IO Bool
confirm = askTheQuestion True

askTheQuestion def question = do
    putStr question
    if def then putStr " [Yn] "
           else putStr " [yN] "
    hFlush stdout

    input <- liftM (map toLower) getLine
    return $ decide def input

decide def [] = def
decide def input
  | and (zipWith (==) "yes" input) = True
  | and (zipWith (==) "no" input) = False 
  | otherwise = def

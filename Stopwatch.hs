module Stopwatch (runStopwatch) where

import Data.Time
import Text.Printf

import Control.Concurrent
import System.Timeout as T
import System.Console.ANSI
import System.IO

data State = Running | Paused | Stopped
  deriving Show

input :: MVar State -> MVar () -> IO ()
input channel window = inputLoop Running channel window

inputLoop :: State -> MVar State -> MVar () -> IO ()
inputLoop Running channel mStdout = do
    char <- getChar
    withMVar mStdout (\_-> putStrLn "")
    case char of
      'p' -> do
          putMVar channel Paused
          inputLoop Paused channel mStdout
      'q' -> do
          putMVar channel Stopped
          inputLoop Stopped channel mStdout
      _ -> inputLoop Running channel mStdout
inputLoop Paused channel mStdout = do
    char <- getChar
    withMVar mStdout (\_-> putStrLn "")
    case char of
      'p' -> do
          putMVar channel Running
          inputLoop Running channel mStdout
      'q' -> do
          putMVar channel Stopped
          inputLoop Stopped channel mStdout
      _ -> inputLoop Paused channel mStdout
inputLoop Stopped _ _ = return ()


output :: MVar State -> MVar () -> IO (NominalDiffTime)
output channel window = outputLoop channel Running (fromIntegral (0 :: Int)) window

outputLoop :: MVar State -> State -> NominalDiffTime -> MVar ()
           -> IO (NominalDiffTime)
outputLoop channel Running amount mStdout = do
    withMVar mStdout (\_ -> clockStatus Running amount)

    start <- getCurrentTime
    maybeState <- T.timeout 1000000 $ takeMVar channel
    stop <- getCurrentTime

    let addAmount = diffUTCTime stop start
    let newAmount = amount + addAmount

    case maybeState of
      Just state -> outputLoop channel state newAmount mStdout
      Nothing -> outputLoop channel Running newAmount mStdout
outputLoop channel Paused amount mStdout = do
    withMVar mStdout (\_ -> clockStatus Paused amount)

    maybeState <- T.timeout 1000000 $ takeMVar channel
    case maybeState of
      Just state -> outputLoop channel state amount mStdout
      Nothing -> outputLoop channel Paused amount mStdout
outputLoop _ Stopped amount mStdout = do
    withMVar mStdout (\_ -> clockStatus Stopped amount)
    return $ amount

clockStatus :: State -> NominalDiffTime -> IO ()
clockStatus state rawAmount = do
    setCursorColumn 0
    clearLine
    putStr $ "Clock " ++ stateString state ++ ": " ++ amountString rawAmount ++ ". "
    putStr $ command state
    hFlush stdout
  where
      stateString Running = "running"
      stateString Paused = "paused"
      stateString Stopped = "stopped"
      amountString amount
        | amount < 60 = show (truncateNominalDiffTime amount) ++ "s"
        | amount < 3600 = show (truncateNominalDiffTime $ amount / 60) ++ "m"
        | otherwise = printf "%.2fh" (nominalDiffTimeToFloat $ amount / 3600)
      command Running = "Press p to pause, q to quit."
      command Paused = "Press p to resume, q to quit."
      command Stopped = ""

nominalDiffTimeToFloat :: NominalDiffTime -> Float
nominalDiffTimeToFloat = fromRational . toRational

truncateNominalDiffTime :: NominalDiffTime -> Int
truncateNominalDiffTime x = truncate . nominalDiffTimeToFloat $ x

runStopwatch :: IO NominalDiffTime
runStopwatch = do
    hSetBuffering stdin NoBuffering
    channel <- newEmptyMVar
    mStdout <- newMVar ()

    _ <- forkIO $ input channel mStdout
    output channel mStdout

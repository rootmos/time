{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Data.Time ( addUTCTime
                 , getCurrentTime
                 )


import TimeData

-------------------------------------------------------------------------------

main :: IO ()
main = do
    con <- connect "127.0.0.1"

    -- insert con (User "Foobar" 7.75)

    foobar <- getUserByNameUnsafe con "Foobar"
    putStrLn (show foobar)

    -- users <- retrieveAllUsers con
    -- mapM (putStrLn . show) users

    close con

--insertUser pipe name = run pipe $ insert (User name 40.0)
--getUnsafe pipe name = liftM head $ run pipe $ find ["name" =: name]
--printUsers pipe = do
--    users <- run pipe $ find [] :: IO [DB User]
--    mapM (putStrLn . show) users
--
--insertRecords pipe user = do
--    now <- getCurrentTime
--    s <- run pipe . insert $ Set (reference user) now 10
--    putStrLn $ show s
--
--    now <- getCurrentTime
--    a <- run pipe . insert $ Add (reference user) now 7
--    putStrLn $ show a
--
--    now <- getCurrentTime
--    b <- run pipe . insert $ Between (reference user) now (addUTCTime 20 now)
--    putStrLn $ show b
--
--printTimeRecords pipe = do
--    records <- run pipe $ find [] :: IO [DB TimeRecord]
--    mapM (putStrLn . show) records
--
--run pipe = access pipe master "time"

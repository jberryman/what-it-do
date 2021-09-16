{-# OPTIONS -fplugin=WhatItDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import WhatItDo (traceDo)
-- main =
--   traceDo (do
--     a <- return ( 42 :: Int )
--     b <- return ( a * 2 )
--     f <- return ( \b -> return ( b - 1 ) )
--     c <- f ( a * b )
--     print c)

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text

announceSleep :: Int -> IO ()
announceSleep n = putStrLn ("Sleeping for secs: "++show n) >> threadDelay (n*1000*1000)


main = do                 -- 17-25
    announceSleep 1
    do announceSleep 1    -- 19-21
       announceSleep 1
       do announceSleep 2 -- 21-21
    n <- return 2
    --doesThisBreak
    anotherDoFunc
    announceSleep n
    anotherTopLevelDoFunc
    runReaderT mreader "string"

    where anotherDoFunc = do -- 27-29
            announceSleep 1
            announceSleep 1

anotherTopLevelDoFunc :: IO ()
anotherTopLevelDoFunc = do     -- 32-33
    announceSleep 2

doesThisBreak :: MonadIO m => m ()
doesThisBreak = do return ()

mreader :: (MonadIO m, MonadReader Text m) => m ()
mreader = do liftIO (announceSleep 5)




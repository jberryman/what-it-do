{-# OPTIONS -fplugin=WhatItDo #-}
import WhatItDo (traceDo)
-- main =
--   traceDo (do
--     a <- return ( 42 :: Int )
--     b <- return ( a * 2 )
--     f <- return ( \b -> return ( b - 1 ) )
--     c <- f ( a * b )
--     print c)

import Control.Concurrent

announceSleep :: Int -> IO ()
announceSleep n = putStrLn ("Sleeping for secs: "++show n) >> threadDelay (n*1000*1000)


main = do                 -- 17-25
    announceSleep 1
    do announceSleep 1    -- 19-21
       announceSleep 1
       do announceSleep 2 -- 21-21
    n <- return 2
    anotherDoFunc
    announceSleep n
    anotherTopLevelDoFunc

    where anotherDoFunc = do -- 27-29
            announceSleep 1
            announceSleep 1

anotherTopLevelDoFunc :: IO ()
anotherTopLevelDoFunc = do     -- 32-33
    announceSleep 2

{-# OPTIONS -fplugin=WhatItDo -ddump-to-file -ddump-tc-ast -O0 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ApplicativeDo #-}
module Main (main, newLogger) where
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
import Data.Text (Text)

announceSleep :: Int -> IO ()
announceSleep n = putStrLn ("Sleeping for secs: "++show n) >> threadDelay (n*1000*1000)


main = do                 -- 17-25
    announceSleep 1
    do announceSleep 1    -- 19-21
       announceSleep 1
       do announceSleep 2 -- 21-21
    n <- return 2
    doesThisBreak
    anotherDoFunc
    announceSleep n
    anotherTopLevelDoFunc
    runReaderT mreader "string"

    runReaderT (x (return ())) ()

    runLogger newLogger

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

class C m where
  x :: m a -> m a

instance MonadIO m => C (ReaderT e m) where
  x k = do
    lift $ do liftIO $ print "tracing"
    k

data Logger = Logger (forall m . MonadIO m => m () )

{-# NOINLINE newLogger #-}
newLogger = Logger $ do liftIO $ putStrLn "logging"

runLogger :: MonadIO m => Logger -> m ()
runLogger (Logger l) = l

data I a = I a deriving Functor


foo :: I Int
foo = do
        x <- I [1,2,3,4]
        return (length x)





{-# OPTIONS -fplugin=WhatItDo -ddump-to-file -ddump-tc-ast -O0 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ApplicativeDo #-}
module Main (main, newLogger) where
import WhatItDo (traceDo)

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text)

import Debug.Trace

announceSleep :: Int -> IO ()
announceSleep n = putStrLn ("Sleeping for secs: "++show n) >> threadDelay (n*1000*1000)


main = do
    announceSleep 1
    do announceSleep 1
       announceSleep 1
       do announceSleep 2
    n <- return 2
    doesThisBreak
    anotherDoFunc
    announceSleep n
    anotherTopLevelDoFunc
    runReaderT mreader "string"

    return $! monomorphicReader "monomorphicReader"

    runReaderT (x (return ())) ()

    runLogger newLogger

    where anotherDoFunc = do
            announceSleep 1
            announceSleep 1

anotherTopLevelDoFunc :: IO ()
anotherTopLevelDoFunc = do
    announceSleep 2

-- No, local constraints don't break (yay!)
doesThisBreak :: MonadIO m => m ()
doesThisBreak = do return ()

-- Local constraints, and we inject code to 'ask' and print the environment:
mreader :: (MonadIO m, MonadReader Text m) => m ()
mreader = do liftIO (announceSleep 5)

-- FIXME our instrumentation isn't effective here:
monomorphicReader :: Text -> ()
monomorphicReader = do
    t <- ask
    !() <- traceM "(inside monomorphicReader)"
    if t == "monomorphicReader" 
       then return () 
       else error "Impossible!"


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

-- ApplicativeDo blocks. This raises a warning for now:
applicativeDoTest :: I Int
applicativeDoTest = do
    x <- I [1,2,3,4]
    return (length x)

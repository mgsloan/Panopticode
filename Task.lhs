> module Task

> import Control.Concurrent
> import Data.Time.Clock.POSIX

This module implements a representation for tasks which must be performed within a restricted amount of time.

> data Task a = Task { creationTime :: Int, priority :: Int,  task :: a -> IO a }

> pureTask :: Int -> (a -> a) -> IO (Task a)
> pureTask pri f = do
>    time <- getCurrentTime
>    return (Task time pri (return . f))

> executeTasks :: Int -> a -> [Task a] -> IO 
> executeTasks us st xs = do
>    time <- getCurrentTime
>    
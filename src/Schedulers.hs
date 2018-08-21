{-
Copyright 2018 Google LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Schedulers where

import Control.Concurrent

import Async

-- * Standard, threaded scheduler

-- | Runs the second action in a separate thread using 'forkIO' and
-- continues with the first one.
threadScheduler :: Scheduler
threadScheduler = Scheduler (\u v -> forkIO v >> u)

-- | Starts an asynchronous computation using 'threadScheduler' and waits for
-- it to finish.
-- This blocks the calling thread until the asynchonous computation finishes (in
-- the same or in a different thread).
runThreadedAndWait :: Async a -> IO a
runThreadedAndWait k = do
  finished <- newEmptyMVar
  unwrapAsync k threadScheduler (putMVar finished)
  takeMVar finished

-- * FIFO, synchronous scheduler

-- | Single-threaded scheduler that runs the actions sequentially, left-one
-- first.
--
-- This one is handy for testing the correctness Async implementation, that is,
-- that no action ever blocks waiting. In particular, if the first action is
-- waiting for the second one in a blocking fashion, it'll deadlock - it must
-- wait on it in a non-blocking way.
fifoScheduler :: Scheduler
fifoScheduler = Scheduler (>>)

-- | Starts an asynchronous computation using the 'fifoScheduler' and waits for
-- it to finish.
runFifo :: Async a -> IO a
runFifo k = do
  finished <- newEmptyMVar
  unwrapAsync k fifoScheduler (putMVar finished)
  m'result <- tryTakeMVar finished
  case m'result of
    Nothing -> error "Some computation failed to call its callback"
    Just result -> return result

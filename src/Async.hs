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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Async
    ( -- * Scheduler
      Scheduler(..)
      -- * Async
    , Async
    , wrapAsync
    , unwrapAsync
      -- * Task
    , Task
    , async
    , await
    ) where

-- See also:
-- "Is there a real-world applicability for the continuation monad outside of
-- academic use?"  -- https://stackoverflow.com/a/41224391/1333025

import Control.Monad.Cont
import Control.Monad.Reader
import Data.IORef

-- * Scheduler

-- | 'Scheduler' knows how to "fork" into two given computations.
-- Some of these computations can be executed just in the resulting @IO ()@
-- action, some in a newly spawned thread, within a thread-pool etc.
-- No constraints are placed on the order or parallelism of their execution.
newtype Scheduler = Scheduler { fork :: IO () -> IO () -> IO () }

-- | Extends 'fork' into a list.
--
--  * If the list is empty, does nothing.
--
--  * If it has just one element, runs it in the resulting @IO ()@ action (no
--    invocation of 'fork').
--
--  * If it has more than one, uses 'fork' to schedule them.
forkMany :: Scheduler -> [IO ()] -> IO ()
forkMany _ [] = return ()
forkMany scheduler ks = foldl1 (fork scheduler) ks

-- * Async

-- | A type for asynchronous IO computations.
-- @Async a@ is equivalent to @Scheduler -> (a -> IO ()) -> IO ()@,
-- as witnessed by 'wrapAsync' and 'unwrapAsync'.
newtype Async a = Async { runAsync :: ReaderT Scheduler (ContT () IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Converts @Scheduler -> (a -> IO ()) -> IO ()@ into @Async a@.
wrapAsync :: (Scheduler -> (a -> IO ()) -> IO ()) -> Async a
wrapAsync f = Async (ReaderT $ ContT . f)

-- | Converts @Async a@ into type @Scheduler -> (a -> IO ()) -> IO ()@
unwrapAsync :: Async a -> Scheduler -> (a -> IO ()) -> IO ()
unwrapAsync k = runContT . runReaderT (runAsync k)

-- * Task state and pure operations on it.

-- | TaskState represents the internal state of a running computation.
data TaskState a
  -- | The computation is still running, with actions waiting for its
  -- result.
  = Running [a -> IO ()]
  -- | The computation has finished with a given result value.
  | Finished a

-- | Creates a new, unfinished 'TaskState'.
newTaskState :: TaskState a
newTaskState = Running []

-- | Registers a callback to be run when the task finishes.
-- If the task has already finished, it returns the applied callback and
-- the caller's responsibility is to invoke it.
whenFinished :: (a -> IO ()) -> TaskState a -> (TaskState a, IO ())
whenFinished f s@(Finished x) = (s, f x)
whenFinished f (Running cs) = (Running (f : cs), return ())

-- | Finishes a 'TaskState' with a given value. The state is switched to
-- 'Finished' and all registered callbacks are returned in a list.
-- The caller's responsibility is to call them all.
finish :: a -> TaskState a -> (TaskState a, [IO ()])
finish _ (Finished _) = error "Finishing twice, this must never happen"
finish x (Running cs) = (Finished x, map ($ x) cs)

-- * Task

-- | Task is a mutable variable holding 'TaskState'.
-- It is created when a new asynchronous computation is started using
-- 'async' and other computations can wait for its completion.
newtype Task a = Task (IORef (TaskState a))

-- | Finishes a given unfinished 'Task' with a given result value.
-- Schedules any tasks that were blocked on it.
finishTask :: Scheduler -> Task a -> a -> IO ()
finishTask scheduler (Task ref) x = do
    ks <- atomicModifyIORef ref (finish x)
    forkMany scheduler ks

-- | Spawns a given computation to run asynchronously using the 'Scheduler'
-- exposed passed to 'Async'.
async :: Async a -> Async (Task a)
async spawn = wrapAsync $ \scheduler c -> do
  task <- Task `liftM` newIORef newTaskState
  fork scheduler
    (c task)
    (unwrapAsync spawn scheduler (finishTask scheduler task))

-- | Waits for a 'Task' to complete and returns its value.
await :: Task a -> Async a
await (Task ref) = wrapAsync $ \_ c ->
  join $ atomicModifyIORef ref (whenFinished c)

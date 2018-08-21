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

module Main where

import Control.Concurrent
import Control.Exception (BlockedIndefinitelyOnMVar(..))
import Control.Monad.IO.Class
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Test.HUnit.Lang (Assertion)

import Async
import Schedulers
import TestUtil

-- | This test demonstrates how a suspended computation can continue elsewhere.
-- In this example, we create a new thread using 'fork' and let the rest of the
-- computation continue there.
testThreadScheduler :: Assertion
testThreadScheduler = do
    threads <- runThreadedAndWait $ do
      startThread <- liftIO myThreadId
      reschedule
      endThread <- liftIO myThreadId
      return (startThread, endThread)
    assertBool ("the computation runs in different threads: " ++ show threads)
               (uncurry (/=) threads)
  where
    reschedule :: Async ()
    reschedule = wrapAsync $ \s cont -> fork s (return ()) (cont ())

-- | Tests that a blocking implementation of async/await is doomed to fail.
-- When a thread block and waits for another, and there is no thread available
-- for the other one to be scheduled, the process will be stuck indefinitely.
--
-- To test this, we use the 'fifoScheduler', which runs the second action
-- passed to 'fork' only after the first one finishes (just like a thread pool
-- with just one thread). GHC runtime is then smart enough to raise an
-- appropriate exception to kill the thread
-- (see https://stackoverflow.com/q/48358718/1333025).
testBlockingAwait :: Assertion
testBlockingAwait =
  assertException BlockedIndefinitelyOnMVar $
    runFifo $ do
      -- Fork a new computation.
      output <- async' $ return (42 :: Int)
      -- And wait for it to finish.
      await' output
  where
    async' :: Async a -> Async (MVar a)
    async' spawn = wrapAsync $ \scheduler cont -> do
      output <- newEmptyMVar
      fork scheduler
        (cont output)
        (unwrapAsync spawn scheduler (putMVar output))
    await' :: MVar a -> Async a
    await' output = wrapAsync $ \_ cont -> takeMVar output >>= cont

-- | Tests that the proper, non-blocking implementation of async/await finishes
-- with the 'fifoScheduler'.
--
-- Note: In this case we know which scheduler to use. Instead, consider
-- implementing a randomized scheduler wrapper that implements Arbitrary and
-- which constructs a scheduler that picks the left or the right argument in
-- 'fork' based on a generated sequence. Then such a scheduler can be passed as
-- an argument in property based tests and allows to check that a specific
-- implementation of a concurrency primitive works regardless of scheduling
-- ordering.
testAwaitAsyncFifo :: Assertion
testAwaitAsyncFifo = do
    result <- runFifo $ do
      -- Fork a new computation.
      output <- async $ return (42 :: Int)
      -- And wait for it to finish.
      await output
    assertEqual "correct result returned" 42 result

main :: IO ()
main = defaultMain
  [ testCase "thread scheduler" testThreadScheduler
  , testCase "blocking await" testBlockingAwait
  , testCase "non-blocking await" testAwaitAsyncFifo
  ]

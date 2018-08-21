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

module TestUtil where

import Control.Exception
import Data.Typeable
import Test.HUnit

-- | Asserts that a given action throws an exception of type @e@.
-- Only the type matters, so it's possible to pass just
-- @(undefined :: MyExceptionType)@.
--
-- Adapted from https://stackoverflow.com/a/6147930/1333025
assertException :: (Exception e) => e -> IO a -> IO ()
assertException ex action =
    handleJust (Just . (`asTypeOf` ex)) (\_ -> return ()) $ do
      _ <- action
      assertFailure $ "Expected exception of type: " ++ show (typeOf ex)

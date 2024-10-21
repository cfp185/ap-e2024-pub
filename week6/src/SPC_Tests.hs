module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC (startSPC, pingSPC, jobAdd, Job(..), JobId)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    localOption (mkTimeout 3000000) $
    testGroup
        "SPC"
            [--testCase "spc" $ do
            --     test <- startSPC
            --     result <- pingSPC test
            --     result @?= 4243,
            --
            -- testCase "ping" $ do
            --     spc <- startSPC
            --     x <- pingSPC spc
            --     x @?= 0
            --     y <- pingSPC spc
            --     y @?= 1
            --     z <- pingSPC spc
            --     z @?= 2,
            --
            testCase "adding job" $ do
                spc <- startSPC
                _ <- jobAdd spc $ Job (pure ()) 1
                pure (),
            --
            testCase "adding job 2" $ do
                spc <- startSPC
                j <- jobAdd spc $ Job (pure ()) 1
                r <- jobStatus spc j
                r @?= Just JobPending
            ]
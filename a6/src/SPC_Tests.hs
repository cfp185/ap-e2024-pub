module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.IORef()
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "add-worker" $ do
          spc <- startSPC
          w1 <- workerAdd spc "worker1"
          -- using case to extract the workerName.
          case w1 of
            Right worker -> workerName worker @?= "worker1"
            Left err -> assertFailure err,
        --
        testCase "worker-same-name" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          w2 <- workerAdd spc "worker1"
          case w2 of
            Right _ -> assertFailure "Expected failure, but got Right"
            Left err -> err @?= "Worker with that name already exists.",
        --
        testCase "multiple-workers" $ do
          spc <- startSPC
          w1 <- workerAdd spc "worker1"
          w2 <- workerAdd spc "worker2"
          case w1 of
            Right worker -> workerName worker @?= "worker1"
            Left err -> assertFailure err
          case w2 of
            Right worker -> workerName worker @?= "worker2"
            Left err -> assertFailure err,
        --
        testCase "infinite-job" $ do
          spc <- startSPC
          w1 <- workerAdd spc "worker1"
          case w1 of
            Right worker -> workerName worker @?= "worker1"
            Left err -> assertFailure err

          let infiniteJob = Job { jobAction = forever (putStrLn "Hello from job!"), jobMaxSeconds = 5 }
          jobId <- jobAdd spc infiniteJob
          status <- jobStatus spc jobId
          status @?= JobRunning

          jobCancel spc jobId
          finalStatus <- jobStatus spc jobId
          finalStatus @?= JobDone DoneCancelled

          let simpleJob = Job { jobAction = putStrLn "Job executed!", jobMaxSeconds = 5 }
          jobId2 <- jobAdd spc simpleJob
          threadDelay 3000
          status2 <- jobStatus spc jobId2
          status2 @?= JobDone Done,
        --
        testCase "infinite-job-timeout" $ do
          spc <- startSPC
          w1 <- workerAdd spc "worker1"
          case w1 of
            Right worker -> workerName worker @?= "worker1"
            Left err -> assertFailure err

          let infiniteJob = Job { jobAction = forever (putStrLn "Hello from job!"), jobMaxSeconds = 0 }
          jobId <- jobAdd spc infiniteJob
          threadDelay 2000000
          status <- jobStatus spc jobId
          status @?= JobDone DoneTimeout

          let simpleJob = Job { jobAction = putStrLn "Job executed!", jobMaxSeconds = 5 }
          jobId2 <- jobAdd spc simpleJob
          threadDelay 3000
          status2 <- jobStatus spc jobId2
          status2 @?= JobDone Done,
        --
        testCase "no-worker-for-job" $ do
          spc <- startSPC
          let job = Job { jobAction = putStrLn "Job executed!", jobMaxSeconds = 5 }
          jobId <- jobAdd spc job
          status <- jobStatus spc jobId
          status @?= JobPending

          w1 <- workerAdd spc "worker1"
          case w1 of
            Right worker -> workerName worker @?= "worker1"
            Left err -> assertFailure err

          threadDelay 1000000
          finalStatus <- jobStatus spc jobId
          finalStatus @?= JobDone Done,
        --
        testCase "exception" $ do
          spc <- startSPC
          w1 <- workerAdd spc "worker1"
          case w1 of
            Right worker -> workerName worker @?= "worker1"
            Left err -> assertFailure err
          let crashingJob = Job { jobAction = error "Job crashed!", jobMaxSeconds = 5 }
          jobId <- jobAdd spc crashingJob
          threadDelay 200000
          status <- jobStatus spc jobId
          status @?= JobDone DoneCrashed

          let simpleJob = Job { jobAction = putStrLn "Job executed!", jobMaxSeconds = 5 }
          jobId2 <- jobAdd spc simpleJob
          threadDelay 3000
          status2 <- jobStatus spc jobId2
          status2 @?= JobDone Done,
        --
        testCase "removing-workers" $ do
          spc <- startSPC
          w1 <- workerAdd spc "worker1"
          case w1 of
            Right worker -> workerName worker @?= "worker1"
            Left err -> assertFailure err

          let simpleJob = Job { jobAction = putStrLn "Job executed!", jobMaxSeconds = 5 }
          jobIdSimple <- jobAdd spc simpleJob
          threadDelay 3000
          statusSimple <- jobStatus spc jobIdSimple
          statusSimple @?= JobDone Done

          let Right stoppedWorker = w1
          workerStop stoppedWorker
          threadDelay 3000

          let simpleJob2 = Job { jobAction = putStrLn "Job executed!", jobMaxSeconds = 5 }
          jobIdSimple2 <- jobAdd spc simpleJob2
          threadDelay 3000
          statusSimple2 <- jobStatus spc jobIdSimple2
          statusSimple2 @?= JobPending

          w2 <- workerAdd spc "worker2"
          threadDelay 3000
          statusSimple3 <- jobStatus spc jobIdSimple2
          statusSimple3 @?= JobDone Done,
        --
        testCase "cancelling-job" $ do
          spc <- startSPC
          w1 <- workerAdd spc "worker1"
          case w1 of
            Right worker -> workerName worker @?= "worker1"
            Left err -> assertFailure err

          let infiniteJob = Job { jobAction = forever (putStrLn "Hello from job!"), jobMaxSeconds = 5 }
          jobId <- jobAdd spc infiniteJob

          let Right stoppedWorker = w1
          workerStop stoppedWorker
          threadDelay 3000

          status1 <- jobStatus spc jobId
          status1 @?= JobDone DoneCancelled
      ]

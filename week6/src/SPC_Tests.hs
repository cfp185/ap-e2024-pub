module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC (startSPC, pingSPC)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    localOption (mkTimeout 3000000) $
    testGroup
        "SPC"
            [testCase "spc" $ do
                test <- startSPC
                result <- pingSPC test
                result @?= 4243,
            --
            testCase
            
            ]

    --  testCase "localEnv" $
    --     runEval
    --       ( localEnv (const [("x", ValInt 1)]) $
    --           askEnv
    --       )
    --       @?= ([], Right [("x", ValInt 1)]),
    --   --
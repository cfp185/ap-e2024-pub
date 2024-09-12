module APL.Eval_Tests (tests) where

import APL.AST (Exp(CstInt))
import APL.Eval (Val(ValInt), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertEqual)

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [successfulTest]
    --, failingTest]


successfulTest :: TestTree
successfulTest = testCase "should work" $ assertEqual "yayyy" (eval (CstInt 5)) $ ValInt 5
--failingTest = testCase "should not work" $ assertEqual "miiiivv" (eval (CstInt 69)) $ ValInt 420





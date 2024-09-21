module APL.Check_Tests (tests) where

import APL.AST (Exp(..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

checkTests :: TestTree
checkTests =
  testGroup
    "Check Expressions"
    [
      testCase "Check valid int constant" $
        checkExp (CstInt 42) @?= Nothing,
      --
      testCase "Check valid bool constant" $
        checkExp (CstBool True) @?= Nothing,
      --
      testCase "Check valid variable" $
        checkExp (Let "x" (CstInt 42) (Var "x")) @?= Nothing,
      --
      testCase "Check undefined variable" $
        checkExp (Var "y") @?= Just "Unknown variable: y",
      --
      testCase "Check addition of two integers" $
        checkExp (Add (CstInt 1) (CstInt 2)) @?= Nothing,
      --
      testCase "Check subtraction with two variables" $
        checkExp (Sub (Var "x") (Var "y")) @?= Just "Unknown variable: x",
      --
      testCase "Check if with a boolean condition" $
        checkExp (If (CstBool True) (CstInt 1) (CstInt 0)) @?= Nothing,
      --
      testCase "Check lambda with valid body" $
        checkExp (Lambda "x" (Add (Var "x") (CstInt 1))) @?= Nothing,
      --
      testCase "Check lambda with undefined variable" $
        checkExp (Lambda "x" (Add (Var "x") (Var "y"))) @?= Just "Unknown variable: y",
      --
      testCase "Check KvPut with valid key-value" $
        checkExp (KvPut (CstInt 0) (CstBool True)) @?= Nothing,
      --
      testCase "Check KvGet with valid key" $
        checkExp (KvGet (CstInt 0)) @?= Nothing,
      --
      testCase "Check KvPut" $
        checkExp (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0))) @?= Nothing
    ]

tests :: TestTree
tests = testGroup "APL Check" [checkTests]
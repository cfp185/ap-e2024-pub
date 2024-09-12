module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [
      testCase "Single expression" $
        printExp (CstInt 1) @?= "1",
      --
      testCase "Single Bool" $
        printExp (CstBool True) @?= "True",
      --
      testCase "Add" $
        printExp (Add (CstInt 1) (CstInt 1)) @?= "(1 + 1)",
      --
      testCase "Sub" $
        printExp (Sub (CstInt 1) (CstInt 1)) @?= "(1 - 1)",
      --
      testCase "Mul" $
        printExp (Mul (CstInt 1) (CstInt 1)) @?= "(1 * 1)",
      --
      testCase "Div" $
        printExp (Div (CstInt 1) (CstInt 1)) @?= "(1 / 1)",
      --
      testCase "Pow" $
        printExp (Pow (CstInt 1) (CstInt 1)) @?= "(1 ** 1)",
      --
      testCase "Nested" $
        printExp (Add (CstInt 1) (Div (CstInt 1) (CstInt 1))) @?= "(1 + (1 / 1))",
      --
      testCase "If expression" $
        printExp (If (Eql (CstInt 1) (CstInt 2)) (CstBool True) (CstBool False))
          @?= "(if (1 == 2) then True else False)",
      --
      testCase "Let expression" $
        printExp (Let "x" (CstInt 2) (Mul (CstInt 5) (CstInt 10)))
          @?= "(let x = 2 in (5 * 10))"
    ]

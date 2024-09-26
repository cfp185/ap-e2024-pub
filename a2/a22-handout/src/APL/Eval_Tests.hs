module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> Either Error Val
eval' e = snd $ runEval (eval e)

-- Function to handle printed output (only used for Print tests)
evalWithOutput :: Exp -> ([String], Either Error Val)
evalWithOutput e = runEval (eval e)

evalTests :: TestTree
evalTests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= Right (ValInt 16),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= Right (ValBool True),
      --
      testCase "TryCatch e1 failure" $
        evalWithOutput
          (TryCatch (Print "x" (Div (CstInt 7) (CstInt 0))) (Print "y" (CstInt 100)))
          @?= (["y: 100"],Right (ValInt 100)),
      --
      testCase "TryCatch e1 failure with Let" $
        evalWithOutput
          (TryCatch (Let "temp" (Print "x" (CstInt 7)) (Div (CstInt 7) (CstInt 0))) (Print "y" (CstInt 100)))
          @?= (["x: 7","y: 100"],Right (ValInt 100)),
      --
      testCase "KvGet invalid key" $
        eval' (KvGet (CstInt 2))
          @?= Left "Invalid key: ValInt 2",
      --
      testCase "KvGet and KvPut ValBool True" $
        eval'(Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0)))
          @?=  Right (ValBool True),
      --
      testCase "KvGet and KvPut Invalid key" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 1)))
          @?= Left "Invalid key: ValInt 1",
      --
      testCase "KvGet and KvPut ValBool False" $
        eval'(Let "y" (KvPut (CstInt 0) (CstBool False)) (KvGet (CstInt 0)))
          @?= Right (ValBool False),
      --
      testCase "Print base case" $
        evalWithOutput (Print "foo" $ CstInt 2)
          @?= (["foo: 2"], Right (ValInt 2)),
      --
      testCase "Print 2 strings" $
        evalWithOutput (Let "x" (Print "foo" $ CstInt 2) (Print "bar" $ CstInt 3))
          @?= (["foo: 2", "bar: 3"], Right (ValInt 3)),
      --
      testCase "Print unknown var" $
        evalWithOutput (Let "x" (Print "foo" $ CstInt 2) (Var "bar"))
          @?= (["foo: 2"], Left "Unknown variable: bar")
      ]


tests :: TestTree
tests = testGroup "APL" [evalTests]

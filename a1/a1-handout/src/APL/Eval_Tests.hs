module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- -- Consider this example when you have added the necessary constructors.
-- -- -- The Y combinator in a form suitable for strict evaluation.
-- yComb :: Exp
-- yComb =
--   Lambda "f" $
--     Apply
--       (Lambda "g" (Apply (Var "g") (Var "g")))
--       (Lambda
--           "g"
--           ( Apply
--               (Var "f")
--               (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
--           )
--       )


-- fact :: Exp
-- fact =
--   Apply yComb $
--     Lambda "rec" $
--       Lambda "n" $
--         If
--           (Eql (Var "n") (CstInt 0))
--           (CstInt 1)
--           (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql int (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql int (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "Eql bool (false)" $
        eval envEmpty (Eql (CstBool False) (CstBool True))
          @?= Right (ValBool False),
      --
      testCase "Eql bool (true)" $
        eval envEmpty (Eql (CstBool True) (CstBool True))
          @?= Right (ValBool True),
      --
      testCase "Eql (invalid)" $
        eval envEmpty (Eql (CstInt 1) (CstBool False))
          @?= Left "Invalid operands to equality",
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "IfThen" $
        eval envEmpty (If (CstBool False) (CstInt 2) (CstInt 7))
          @?= Right (ValInt 7),
      --
      testCase "IfNonBoolean" $
        eval envEmpty (If (CstInt 6) (CstInt 2) (CstInt 7))
          @?= Left "Non-boolean conditional.",
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "Lambda 1.0" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 4))
            @?= Right (ValInt 5),
      --
      testCase "Lambda 2.0" $
        eval
          envEmpty
          ( Let
              "x" (CstInt 2)
              (Lambda "y" (Add (Var "x") (Var "y"))))
                @?= Right (ValFun [("x", ValInt 2)] "y" (Add (Var "x") (Var "y"))),
      --
      testCase "Lambda - FunExp failing" $
        eval
          envEmpty
            (Apply ( Let
              "x" (CstInt 2)
              (Lambda "y" (Div (CstInt 2) (CstInt 0)))) (CstInt 3))
                @?= Left "Division by zero",
      --
      testCase "Lambda - ArgExp failing" $
        eval
          envEmpty
            (Apply ( Let
              "x" (CstInt 2)
              (Lambda "y" (Add (CstInt 2) (CstInt 0)))) (Pow (CstInt 2) (CstInt (-5))))
                @?= Left "Negative exponent",
      --
      testCase "Lambda - BothExp failing showing order" $
        eval
          envEmpty
            (Apply ( Let
              "x" (CstInt 2)
              (Lambda "y" (Div (CstInt "x") (CstInt 0)))) (Pow (CstInt 2) (CstInt (-5))))
                @?= Left "Negative Exponent",
      --
      testCase "Apply" $
        eval
          envEmpty
            (Apply
              ( Let "x" (CstInt 2)
                (Lambda "y" (Add (Var "x") (Var "y")))) ((CstInt 2)))
                  @?= Right (ValInt 4),
      --
      testCase "Apply - Failing with function value" $
        eval
          envEmpty
            (Apply
              (Add (CstInt 2) (CstInt 2)) (CstInt 3))
                @?= Left "Trying to apply a non-function value",
      --
      testCase "TryCatch" $
        eval
          envEmpty
            (TryCatch (CstInt 0) (CstInt 1))
              @?= Right (ValInt 0),
      --
      testCase "TryCatch" $
        eval
          envEmpty
            (TryCatch (Div (CstInt 2) (CstInt 0)) (CstInt 1))
              @?= Right (ValInt 1)
    ]

module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      -- Test Let expression
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      -- Test shadowing in Let
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      -- Test state manipulation
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      -- Test PrintOp
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      -- Test error handling
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      -- Test division by zero
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      -- Test TryCatch success
      testCase "Success!" $
        runEval (Free $ TryCatchOp (failure "Oh no!") (pure "Success!"))
          @?= ([], Right "Success!"),
      -- Test TryCatch with a safe operation
      testCase "divZero" $
        eval' (TryCatch (CstInt 5) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Right (ValInt 5)),
      -- Test KvPutOp and KvGetOp
      testCase "KvPutOp & KvGetOp" $
        runEval (Free $ (KvPutOp (ValInt 0) (ValInt 1)) (Free $ KvGetOp (ValInt 0) $ \val -> pure val))
          @?= ([],Right (ValInt 1)),
      -- Test KvGetOp when the key does not exist
      testCase "KvGetOp key not found" $
        runEval (Free $ KvGetOp (ValInt 999) $ \_ -> pure (ValInt 42))
          @?= ([], Left "Key not found: ValInt 999")
    ]


ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
      -- Test error handling with division by zero
      testCase "badEql" $ do
        let expr = TryCatch (Eql (CstInt 0) (CstBool True)) (Div (CstInt 1) (CstInt 0))
        result <- runEvalIO (eval expr)
        result @?= Left "Division by zero",
      -- Test KvPutOp and KvGetOp
      testCase "KvPutOp and KvGetOp" $ do
        result <- runEvalIO $ do
          Free $ KvPutOp (ValInt 0) (ValInt 42) $ do
            Free $ KvGetOp (ValInt 0) pure
        result @?= Right (ValInt 42),
      -- Test KvGetOp when the key does not exist
      testCase "KvGetOp non-existing key" $ do
        result <- runEvalIO $ do
          Free $ KvGetOp (ValInt 999) pure
        result @?= Left "Key not found: ValInt 999",
      -- Test StatePutOp and StateGetOp
      testCase "StatePutOp and StateGetOp" $ do
        result <- runEvalIO $ do
          Free $ StatePutOp [(ValInt 0, ValInt 42)] $ do
            Free $ StateGetOp pure
        result @?= Right [(ValInt 0, ValInt 42)],
      -- Test TryCatchOp for successful recovery
      testCase "TryCatchOp success" $ do
        result <- runEvalIO $ do
          Free $ TryCatchOp (Free $ ErrorOp "Test error") (pure (ValInt 10))
        result @?= Right (ValInt 10)

        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]

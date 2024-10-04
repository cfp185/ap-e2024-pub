module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
      in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' r s (Free (TryCatchOp m1 m2)) =
      let (logs1, res1) = runEval' r s m1
      in case res1 of
          Left _ -> runEval' r s m2
          Right _ -> (logs1, res1)
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Just val -> runEval' r s (k val)
        Nothing -> ([], Left $ "Key not found: " ++ show key)
    runEval' r s (Free (KvPutOp key val m)) =
      let newState = (key, val) : filter (\(k, _) -> k /= key) s
      in runEval' r newState m
    runEval' r s (Free (TransactionOp e a)) =
      let (str, res) = runEval' r s e
      in case res of
        Left _ -> let (str', res') = runEval' r s a
                  in (str ++ str', res')
        Right _ -> runEval' r s a






-- ([],Left "Key not found: ValInt 0")

-- ([],Right (ValInt 1))


-- :m *APL.Eval *APL.AST *APL.InterpPure *APL.InterpIO




-- > goodPut = evalKvPut (ValInt 0) (ValInt 1)
-- > badPut = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
-- > get0 = KvGet (CstInt 0)
-- > runEval $ transaction goodPut >> eval get0
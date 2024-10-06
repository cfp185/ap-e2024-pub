module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval evalm =
  let (_, str, result) = runEval' envEmpty stateInitial evalm
  in (str, result)
  where
    runEval' :: Env -> State -> EvalM a -> (State, [String], Either Error a)
    runEval' _ s (Pure x) = (s, [], Right x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (s', ps, res) = runEval' r s m
      in (s', p : ps, res)
    runEval' _ s (Free (ErrorOp e)) = (s, [], Left e)
    runEval' r s (Free (TryCatchOp m1 m2)) =
      let (s', str', res') = runEval' r s m1
      in case res' of
          Left _ -> runEval' r s m2
          Right _ -> (s', str', res')
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Just val -> runEval' r s (k val)
        Nothing -> (s, [], Left $ "Invalid key: " ++ show key)
    runEval' r s (Free (KvPutOp key val m)) =
      let newState = (key, val) : filter (\(k, _) -> k /= key) s
      in runEval' r newState m
    runEval' r s (Free (TransactionOp e a)) =
      let (s', str', res') = runEval' r s e
      in case res' of
        Left _ ->
          let (s'', str'', res'') = runEval' r s a
          in (s'', str' ++ str'', res'')
        Right _ ->
          let (s'', str'', res'') = runEval' r s' a
          in (s'', str' ++ str'', res'')
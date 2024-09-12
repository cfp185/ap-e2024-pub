module APL.Eval
  (
    Val (ValInt), eval
  )
where
  import APL.AST (Exp (CstInt, Add, Sub, Mul, Div, Pow, CstBool, Eql, If, Var, Let)
                 , Error
                 , VName)

  data Val
    = ValInt Integer
    | ValBool Bool
    | ValFun Env VName Exp
    deriving (Eq, Show)

  type Env = [(VName, Val)]

  --eval :: Exp -> Either Error Val
  eval :: Env -> Exp -> Either Error Val                 -- example usage: eval (CstInt 2)
  eval env (Var vname) =
    case (envLookup vname env) of
      Just x -> Right $ x
      Nothing -> Left $ error ("miivvvvvv variable does not exist: " ++ vname)
  eval env (Let var e1 e2) =
    case eval env e1 of
      Left err -> Left $ error "miv variable exists in the environment"
      Right v -> eval (envExtend var v env) e2
  eval env (CstInt ex) = Right $ ValInt ex
  eval env (Add e1 e2) =                                 -- example usage: eval (Add (CstInt 2) (CstInt 2))
    case (eval env e1, eval env e2) of
      (Right (ValInt v1), Right (ValInt v2)) -> Right $ ValInt (v1 + v2)
      (_, Left err) -> Left err
      (Left err, _ ) -> Left err
  eval env (Sub e1 e2) =                                 -- example usage: eval (Sub (CstInt 2) (CstInt 2))
    case(eval env e1, eval env e2) of
      (Right (ValInt v1), Right (ValInt v2)) -> Right $ ValInt (v1 - v2)
      (_, Left err) -> Left err
      (Left err, _ ) -> Left err
  eval env (Mul e1 e2) =                                 -- example usage: eval (Mul (CstInt (-2)) (CstInt 10))
    case(eval env e1, eval env e2) of
      (Right (ValInt v1), Right (ValInt v2)) -> Right $ ValInt (v1 * v2)
      (_, Left err) -> Left err
      (Left err, _ ) -> Left err
  eval env (Div e1 e2) =                                 -- example usage: eval (Div (CstInt 10) (CstInt 2))
    case(eval env e1, eval env e2) of
      (Right (ValInt v1), Right (ValInt v2)) ->
        if v2 == 0
          then Left $ error "division by 0 miv"
        else Right $ ValInt (v1 `div` v2)
      (_, Left err) -> Left err
      (Left err, _ ) -> Left err
  eval env (Pow e1 e2) =                                 -- example usage: eval (Add (CstInt 2) (CstInt 2))
    case(eval env e1, eval env e2) of
      (Right (ValInt v1), Right (ValInt v2)) ->
        if v2 > 0
          then Right $ ValInt (v1 ^ v2) -- normal case
        else if v2 == 0
          then Left $ error "negative exponent miivvvvvv"
        else
          Right $ ValInt (1 `div` (v1 ^ (abs v2)))      -- negative case (not representable as an integer).
      (_, Left err) -> Left err
      (Left err, _ ) -> Left err
  eval env (CstBool ex) = Right $ ValBool ex            -- example usage: eval (CstBool True)
  eval env (Eql e1 e2) =                                -- example usage: eval (Eql (CstInt 2) (CstInt 2))
    case(eval env e1, eval env e2) of
      (Right (ValInt v1), Right (ValInt v2)) ->
        if v1 == v2
          then Right $ ValBool True
        else Right $ ValBool False
      (_, Left err) -> Left err
      (Left err, _ ) -> Left err
  eval env (If e1 e2 e3) =                              -- example usage: eval (If (Eql (CstInt 22) (CstInt 2)) (Add (CstInt 3) (CstInt 3)) (CstInt 0))
    case(eval env e1, e2, e3) of
      (Right (ValBool v1), _, _) ->
        if ValBool v1 == ValBool True
          then eval env e2
        else
          eval env e3
      (Left err, _, _) -> Left err

  -- Empty environment, which contains no variable bindings.
  envEmpty :: Env                                   -- example usage: envName = envEmpty
  envEmpty = []

  -- Extend an environment with a new variable binding, producing a new environment.
  envExtend :: VName -> Val -> Env -> Env
  envExtend name val env = env ++ [(name, val)]

  -- Look up a variable name in the provided environment.
  -- Returns Nothing if the variable is not in the environment.

  envLookup :: VName -> Env -> Maybe Val
  envLookup key env =
    case env of
      [] -> Nothing
      ((x, y) : xys) ->
        if key == x
          then Just y
        else
          envLookup key xys




-- EXAMPLE USAGE OF ENVIRONMENTS:
--ghci> diku = envEmpty
--ghci> envLookup "x" diku
--Nothing
--ghci> diku2 = envExtend "sporring" (ValBool False) diku
--ghci> diku2
--[("sporring",ValBool False)]
--ghci> envLookup "sporring" diku2
--Just (ValBool False)
--ghci> envLookup "dinmor" diku2
--Nothing

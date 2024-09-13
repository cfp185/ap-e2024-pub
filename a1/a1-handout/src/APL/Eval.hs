module APL.Eval
  ( Val (..),
    Env,
    envEmpty,
    eval,
  )
where

import APL.AST (Exp (..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

evalIntBinOp :: (Integer -> Integer -> Either Error Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp f env e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> case f x y of
      Left err -> Left err
      Right z -> Right $ ValInt z
    (Right _, Right _) -> Left "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp' f env e1 e2 =
  evalIntBinOp f' env e1 e2
  where
    f' x y = Right $ f x y

eval :: Env -> Exp -> Either Error Val
eval _env (CstInt x) = Right $ ValInt x
eval _env (CstBool b) = Right $ ValBool b
eval env (Var v) = case envLookup v env of
  Just x -> Right x
  Nothing -> Left $ "Unknown variable: " ++ v
eval env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2
eval env (Div e1 e2) = evalIntBinOp checkedDiv env e1 e2
  where
    checkedDiv _ 0 = Left "Division by zero"
    checkedDiv x y = Right $ x `div` y
eval env (Pow e1 e2) = evalIntBinOp checkedPow env e1 e2
  where
    checkedPow x y =
      if y < 0
        then Left "Negative exponent"
        else Right $ x ^ y
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Non-boolean conditional."
eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2
eval env (Lambda vname e1) = Right $ (ValFun env vname e1)      -- eval [] (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))
eval env (Apply funExp argExp) =
  case (eval env funExp, eval env argExp) of
    --(_, Left err) -> Left err
    (Left err, _) -> Left err    -- If evaluating the function fails   eval [] (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
    (_, Left err) -> Left err   -- If evaluating the argument fails
    (Right (ValFun env' vname e1), Right v) ->
      eval (envExtend vname v env') e1  -- Apply the function
    (Right _, _) -> Left "Trying to apply a non-function value"  -- If funExp is not a function
eval env (TryCatch e1 e2) =
  case eval env e1 of
    Left _ -> eval env e2
    Right v -> Right $ v

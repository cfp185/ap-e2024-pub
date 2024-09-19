module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
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

-- Takes: an environment (Env) and a list of printed string ([String]).
-- Returns: either an error (Left, (Error, [String])) or a result (Right, (a, [String])), while keeping the printed state in both cases.
-- The environment allows for variable lookup, while the list of printed strings accumulates all print statements as the evaluation progresses.
newtype EvalM a = EvalM (Env -> [String] -> Either (Error, [String]) (a, [String]))

instance Functor EvalM where
  fmap f (EvalM m) = EvalM $ \env prints ->
    case m env prints of
      Left err -> Left err
      Right (x, prints') -> Right (f x, prints')

instance Applicative EvalM where
  pure x = EvalM $ \_env prints -> Right (x, prints)
  EvalM f <*> EvalM x = EvalM $ \env prints ->
    case f env prints of
      Left err -> Left err
      Right (f', prints') -> case x env prints' of
        Left err -> Left err
        Right (x', prints'') -> Right (f' x', prints'')

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env prints ->
    case x env prints of
      Left err -> Left err
      Right (x', newPrints) ->
        let EvalM y = f x'
        in y env newPrints

getPrints :: EvalM [String]
getPrints = EvalM $ \_ prints -> Right (prints, prints)

putPrints :: [String] -> EvalM ()
putPrints newPrints = EvalM $ \_ _ -> Right ((), newPrints)

-- 'modifyPrints' and 'evalPrint' has been implemented to manage the printed output.
-- These two functions are responsible for ensuring that any time a 'Print' statement is evaluated, the string is properly accumulated in the list of printed output.

-- Modifies the list of printed strings. It can for instance append new strings to the list as print statements are encountered.
modifyPrints :: ([String] -> [String]) -> EvalM ()
modifyPrints f = EvalM $ \_ prints -> Right ((), f prints)

-- Updated runEval to ensure that both the Right and Left cases in the monad propagate the state of printed messages correctly.
-- Right case: return both the result and the accumulated printed strings.
-- Left case: return the printed strings accumulated before the failed occured.

-- Initializes the evaluation with an empty environment (envEmpty) and an empty list of printed strings ([]).
-- If an error occurs (Left (err, prints)), we return both the error and the printed strings up to that point.
-- If the evaluation succeeds (Right (result, prints)), we return the result and the printed strings.
runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
  case m envEmpty [] of
    Left (err, prints) -> (prints, Left err)  -- Return prints in case of error
    Right (result, prints) -> (prints, Right result)

-- Generates a formatted string from the value (Val) being printed and appends it to the list of printed strings using 'modifyPrints'.
evalPrint :: String -> Val -> EvalM ()
evalPrint s v = do
  let output = s ++ ": " ++ showVal v
  modifyPrints (++ [output])  -- Append the output to the list of prints

showVal :: Val -> String
showVal (ValInt i) = show i
showVal (ValBool b) = show b
showVal (ValFun _ _ _) = "#<fun>"

askEnv :: EvalM Env
askEnv = EvalM $ \env prints -> Right (env, prints)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

-- When failure is activated, it returns a Left value, indicating an error, but it also ensures that any printed strings are returned as well (Left (s, prints)).
-- This prevents any printed output from being lost when an error occurs.
failure :: String -> EvalM a
failure s = EvalM $ \_env prints -> Left (s, prints)

-- Designed to handle errors gracefully by trying an alternative expression if an error occurs.
-- Tries the computation m1. If it fails (Left), it returns the second computation m2, using the same list of printed strings (prints).
-- If the first computation succeeds, it simply returns the result and the updated list of printed strings.
catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env prints ->
  case m1 env prints of
    Left _ -> m2 env prints  -- If m1 fails, try m2 with the same printed state
    Right (x, prints') -> Right (x, prints')  -- If m1 succeeds, keep its result and state


evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v -- if the variable is not found, failure is invoked, which returns an error, but crucially, the printed output that occurred before the error is preserved.
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print s e) = do
    v <- eval e  -- First evaluate the expression `e` to get its value.
    evalPrint s v -- Use the helper function to format and add the printed string.
    return v  -- Return the value as per the assignment.

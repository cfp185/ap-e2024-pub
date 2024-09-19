module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

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

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State t) s = t s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

instance Functor (State s) where
    -- fmap f x = x >>= \x' -> pure (f x')
    fmap f (State t) = State $ \s ->
        let (x, s') = t s
        in (f x, s')
    -- f :: a -> b
    -- x :: a
    -- t :: s -> (a, s)

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    -- (<*>) :: State s (a -> b) -> State s a -> State s b
    State tf <*> State tx = State $ \s ->
        let (f, s') = tf s
            (x, s'') = tx s'
        in (f x, s'')

instance Monad (State s) where
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    State tx >>= f = State $ \s ->
        let (x, s') = tx s
            State ty = f x
        in ty s'

newtype EvalM a = EvalM (Env -> State [String] (Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env -> State $ \st -> (Right x, st)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env -> State $ \st ->
    let (result, st') = runState (x env) st
    in case result of
      Left err -> (Left err, st')
      Right x' -> runState ((let EvalM y = f val in y env)) st'

evalPrint :: String -> EvalM ()
evalPrint s = EvalM $ \_env -> State $ \st ->
  (Right (), st ++ [s])  -- Append the string to the list of printed strings

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
  let (result, finalState) = runState (m envEmpty) []
  in (finalState, result)

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
    Nothing -> failure $ "Unknown variable: " ++ v
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
eval (Print msg exp) = do
  val <- eval exp
  let strVal = case val of
    ValInt x -> show x
    ValBool b -> show b
    ValFun _ _ _ -> "#<fun>"
  in evalPrint (msg ++ ": " ++ strVal)
  pure val

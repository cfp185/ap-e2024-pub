module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import Control.Applicative (ZipList(getZipList))

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

-- 

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

stateEmpty :: State
stateEmpty = ([],[])

type Error = String

type KeyVal = [(Val, Val)]

type State = ([String], KeyVal)

newtype EvalM a = EvalM (Env -> State -> (State, Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \env s -> (s, Right x)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env st ->
    case x env st of
      (st, Left err) -> (st, Left err)
      (st, Right x') ->
        let EvalM y = f x'
        in y env st

askEnv :: EvalM Env
askEnv = EvalM $ \env st -> (st, Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env st -> (st, Left s)

-- catch :: EvalM a -> EvalM a -> EvalM a
-- catch (EvalM m1) (EvalM m2) = EvalM $ \env st ->
--   case m1 env st of
--     (_, Left _) -> m2 env st
--     (_ , Right x) -> (st, Right x)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env st ->
  case m1 env st of
    (newSt, Left _) -> m2 env newSt
    (newSt, Right x) -> (newSt, Right x)

evalPrint :: String -> Val -> EvalM ()
evalPrint s v = EvalM $ \_env (stList, kval) ->
  let st = s ++ ": " ++ showVal v
    in ((stList ++ [st], kval), Right ())

showVal :: Val -> String
showVal (ValInt i) = show i
showVal (ValBool b) = show b
showVal (ValFun {}) = "#<fun>"

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) = do
  case m envEmpty stateEmpty of
    ((sList, key), Left err) -> (sList, Left err)
    ((sList, key), Right m') -> (sList, Right m')

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f =
  evalIntBinOp f'
  where
    f' x y = pure $ f x y

evalKvGet :: Val -> EvalM Val
evalKvGet k = do
  (_, kval) <- EvalM $ \_env st -> (st, Right st)
  case lookup k kval of
    Just v -> pure v
    Nothing  -> failure $ "Invalid key: " ++ show k


evalKvPut :: Val -> Val -> EvalM ()
evalKvPut k v = do
  (stList, kval) <- EvalM $ \_env st -> (st, Right st)
  let kval' = case lookup k kval of
                Just _  -> map (\(k', v') -> if k' == k then (k, v) else (k', v')) kval
                Nothing -> (k, v) : kval
  EvalM $ \_env _ -> ((stList, kval'), Right ())

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
eval (Print s e) = do
    v <- eval e  -- First evaluate the expression `e` to get its value.
    evalPrint s v
    pure v-- Return the value as per the assignment.
eval (KvPut kExp vExp) = do
  k <- eval kExp
  v <- eval vExp
  evalKvPut k v
  pure v
eval (KvGet exp) = do
  k <- eval exp
  evalKvGet k


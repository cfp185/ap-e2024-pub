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

newtype EvalM a = EvalM (Env -> Either Error a)

instance Monad EvalM where
    EvalM x >>= f = EvalM $ \env ->
      case x env of
        Left err -> Left err
        Right x' ->
          let EvalM y = f x'
            in y env

instance Applicative EvalM where
  pure x = EvalM $ \_env -> Right x

  EvalM ef <*> EvalM ex = EvalM $ \env ->
    case (ef env, ex env) of
      (Left err, _)  -> Left err
      (_, Left err) -> Left err
      (Right f, Right x) -> Right (f x)

instance Functor EvalM where
  fmap f (EvalM x) =
    EvalM $ \env -> case x env of
      Right v -> Right $ f v
      Left err -> Left err

failure :: String -> EvalM a
--failure s = EvalM $ Left s
failure s = EvalM $ \env -> Left s

catch :: EvalM a -> EvalM a -> EvalM a
--catch (EvalM m1) (EvalM m2) = EvalM $
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
                      case (m1 env) of
                        Left _ -> m2 env
                        Right m1' -> Right m1'

runEval :: EvalM a -> EvalM (Env -> Either Error a)
runEval (EvalM x) = EvalM $ \env -> Right x

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv temp_env (EvalM e) = EvalM $ \env -> e (temp_env env)

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool x) = pure $ ValBool x
eval (Var vname) = do
                      env <- askEnv
                      case envLookup vname env of
                        Just x -> pure x
                        Nothing -> failure $ ("Non-existing variable: " ++ vname)
eval (Add e1 e2) = do
                        x <- eval e1
                        y <- eval e2
                        case (x, y) of
                          (ValInt x', ValInt y') -> pure $ ValInt $ x'+ y'
                          _ -> failure "Non-integer operand"
eval (Sub e1 e2) = do
                        x <- eval e1
                        y <- eval e2
                        case (x ,y) of
                          (ValInt x', ValInt y') -> pure $ ValInt $ x' - y'
                          _ -> failure "Non-integer operand"
eval (Mul e1 e2) = do
                        x <- eval e1
                        y <- eval e2
                        case (x ,y) of
                          (ValInt x', ValInt y') -> pure $ ValInt $ x' * y'
                          _ -> failure "Non-integer operand"
eval (Div e1 e2) = do
                        x <- eval e1
                        y <- eval e2
                        case (x ,y) of
                          (ValInt x', ValInt 0) -> failure "Div 0 error"
                          (ValInt x', ValInt y') -> pure $ ValInt $ x' `div` y'
                          _ -> failure "Non-integer operand"
eval (Pow e1 e2) = do
                        x <- eval e1
                        y <- eval e2
                        case (x ,y) of
                          (ValInt x', ValInt y') ->
                            if y' < 0
                              then failure "Negative exponent"
                              else pure $ ValInt $ x' ^ y'
                          _ -> failure "Non-integer operand"
eval (Eql e1 e2) =  do
                        x <- eval e1
                        y <- eval e2
                        case (x, y) of
                          (ValInt x', ValInt y') -> -- || ((ValBool x', ValBool y')) ->
                            if x' == y'
                              then pure $ ValBool True
                            else pure $ ValBool False
                          _ -> failure "Non-integer operand"
eval (If cond e1 e2) =  do
                        cond' <- eval cond
                        case cond' of
                          (ValBool True) -> eval e1
                          (ValBool False) -> eval e2
                          _ -> failure "Non-boolean operand"
eval (Let var e1 e2) = do
                        e1' <- eval e1
                        localEnv (envExtend var e1') $ eval e2
eval (Lambda vname e1) = do
                          env <- askEnv
                          pure $ (ValFun env vname e1)
eval (Apply funExp argExp) = do
                        funExp' <- eval funExp
                        argExp' <- eval argExp
                        case (funExp', argExp') of
                          (ValFun f_env var body, arg) ->
                            localEnv (const $ envExtend var arg f_env) $ eval body
                          (_, _) ->
                            failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
                        eval e1 `catch` eval e2


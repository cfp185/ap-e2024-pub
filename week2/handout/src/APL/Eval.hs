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


newtype EvalM a = EvalM (Either Error a)
  deriving Show

instance Monad EvalM where
  EvalM x >>= f = EvalM $ case x of
    Left err -> Left err
    Right x' ->
      let EvalM y = f x'
        in y

instance Applicative EvalM where
  pure x                              = EvalM $ Right x
  EvalM (Left e)  <*> _               = EvalM (Left e)
  _               <*> EvalM (Left e)  = EvalM (Left e)
  EvalM (Right f) <*> EvalM (Right x) = EvalM (Right $ f x)

instance Functor EvalM where
  fmap _ (EvalM (Left e))  = EvalM $ Left e
  fmap f (EvalM (Right x)) = EvalM $ Right $ f x

failure :: String -> EvalM a
failure s = EvalM $ Left s

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $
                      case m1 of
                        Left _ -> m2
                        Right m1 -> Right m1

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x

eval :: Env -> Exp -> EvalM Val
eval _ (CstInt x) = pure $ ValInt x
eval _ (CstBool x) = pure $ ValBool x
eval env (Var vname) = do
                        case (envLookup vname env) of
                          Just x -> pure x
                          Nothing -> failure ("Non-existing variable: " ++ vname)
eval env (Add e1 e2) = do
                        x <- eval env e1
                        y <- eval env e2
                        case (x, y) of
                          (ValInt x', ValInt y') -> pure $ ValInt $ x'+ y'
                          _ -> failure "Non-integer operand"
eval env (Sub e1 e2) = do
                        x <- eval env e1
                        y <- eval env e2
                        case (x ,y) of
                          (ValInt x', ValInt y') -> pure $ ValInt $ x' - y'
                          _ -> failure "Non-integer operand"
eval env (Mul e1 e2) = do
                        x <- eval env e1
                        y <- eval env e2
                        case (x ,y) of
                          (ValInt x', ValInt y') -> pure $ ValInt $ x' * y'
                          _ -> failure "Non-integer operand"
eval env (Div e1 e2) = do
                        x <- eval env e1
                        y <- eval env e2
                        case (x ,y) of
                          (ValInt x', ValInt 0) -> failure "Div 0 error"
                          (ValInt x', ValInt y') -> pure $ ValInt $ x' `div` y'
                          _ -> failure "Non-integer operand"
eval env (Pow e1 e2) = do
                        x <- eval env e1
                        y <- eval env e2
                        case (x ,y) of
                          (ValInt x', ValInt y') ->
                            if y' < 0
                              then failure "Negative exponent"
                              else pure $ ValInt $ x' ^ y'
                          _ -> failure "Non-integer operand"
eval env (Eql e1 e2) =  do
                        x <- eval env e1
                        y <- eval env e2
                        case (x, y) of
                          (ValInt x', ValInt y') -> -- || ((ValBool x', ValBool y')) ->
                            if x' == y'
                              then pure $ ValBool True
                            else pure $ ValBool False
                          _ -> failure "Non-integer operand"
eval env (If cond e1 e2) =  do
                        cond' <- eval env cond
                        case cond' of
                          (ValBool True) -> eval env e1
                          (ValBool False) -> eval env e2
                          _ -> failure "Non-boolean operand"
eval env (Let var e1 e2) = do
                        e1' <- eval env e1
                        eval (envExtend var e1' env) e2
eval env (Lambda vname e1) = pure $ (ValFun env vname e1)
eval env (Apply funExp argExp) = do
                        funExp' <- eval env funExp
                        argExp' <- eval env argExp
                        case (funExp', argExp') of
                          (ValFun env' vname e1, v) ->
                            eval (envExtend vname v env') e1  -- Apply the function
                          ( _, _) -> failure "Trying to apply a non-function value"
eval env (TryCatch e1 e2) =
                        eval env e1 `catch` eval env e2


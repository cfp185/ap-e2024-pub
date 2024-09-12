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
  EvalM x >>= f = case of
    Nothing -> EvalM (Left e)

  envLookup :: VName -> Env -> Maybe Val
  envLookup key env =
    case env of
      [] -> Nothing
      ((x, y) : xys) ->
        if key == x
          then Just y
        else
          envLookup key xys

instance Applicative EvalM where
  pure x                              = EvalM $ Right x
  EvalM (Left e)  <*> _               = EvalM (Left e)
  _               <*> EvalM (Left e)  = EvalM (Left e)
  EvalM (Right f) <*> EvalM (Right x) = EvalM (Right $ f x)

instance Functor EvalM where
  fmap _ (EvalM (Left e))  = EvalM $ Left e
  fmap f (EvalM (Right x)) = EvalM $ Right $ f x

runEval :: EvalM a -> Either Error a
runEval = undefined -- TODO


eval :: Env -> Exp -> EvalM Val
eval = undefined -- TODO

--class Functor f where
  --fmap :: (a -> b) -> (f a -> f b)

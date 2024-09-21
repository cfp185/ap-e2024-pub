module APL.Check (checkExp, Error, CheckM) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Env = [VName]

type Error = String

newtype CheckM a = CheckM (Env -> Either Error a)

instance Functor CheckM where
    fmap = liftM

instance Applicative CheckM where
    pure x = CheckM $ \_ -> Right x
    (<*>) = ap

instance Monad CheckM where
    CheckM x >>= f = CheckM $ \env ->
        case x env of
            Left err -> Left err
            Right x' ->
                let CheckM y = f x'
                in y env

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

localEnv :: (Env -> Env) -> CheckM a -> CheckM a
localEnv f (CheckM m) = CheckM $ \env -> m (f env)

failure :: String -> CheckM a
failure s = CheckM $ \_env -> Left s

runCheck :: CheckM a -> (Env -> Either Error a)
runCheck (CheckM m) = m

check :: Exp -> CheckM ()
check (CstInt _) = pure ()
check (CstBool _) = pure ()
check (Var v) = do
    env <- askEnv
    if elem v env -- elem = se om v er i en
    then pure ()
    else failure $ "Unknown variable: " ++ v
check (Add e1 e2) = check e1 >> check e2
check (Sub e1 e2) = check e1 >> check e2
check (Mul e1 e2) = check e1 >> check e2
check (Div e1 e2) = check e1 >> check e2
check (Pow e1 e2) = check e1 >> check e2
check (Eql e1 e2) = check e1 >> check e2
check (If cond e1 e2) = check cond >> check e1 >> check e2
check (Let var e1 e2) = do
    --v1 <- check e1
    check e1
    localEnv (var:) $ check e2 -- var: = liste kun med var
check (Lambda var body) = do
    localEnv (var:) $ check body
check (Apply e1 e2) = check e1 >> check e2
check (TryCatch e1 e2) = check e1 >> check e2
check (Print s e) = check e
check (KvPut kExp vExp) = check kExp >> check vExp
check (KvGet exp) = check exp

checkExp :: Exp -> Maybe Error
checkExp exp =
    case runCheck (check exp) [] of
        Left err -> Just err
        Right _ -> Nothing

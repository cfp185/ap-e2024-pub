module APL.AST
  ( VName,
    Exp (..),
    printExp
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

printExp :: Exp -> String
printExp (CstInt e1)  = show e1
printExp (CstBool e1) = show e1
printExp (If e1 e2 e3) = "(if " ++ printExp e1 ++ " then " ++ printExp e2 ++ " else " ++ printExp e3 ++ ")"
printExp (Let vname e1 e2) = "(let " ++ vname ++ " = " ++ printExp e1 ++ " in " ++ printExp e2 ++ ")"
printExp (Lambda vname e1) = "(\\" ++  vname ++ " -> " ++ printExp e1 ++ ")"
printExp exp' = case exp' of
    Add e1 e2 -> twoExp e1 e2 "+" ""
    Sub e1 e2 -> twoExp e1 e2 "-" ""
    Mul e1 e2 -> twoExp e1 e2 "*" ""
    Div e1 e2 -> twoExp e1 e2 "/" ""
    Pow e1 e2 -> twoExp e1 e2 "**" ""
    Eql e1 e2 -> twoExp e1 e2 "==" ""
    Apply e1 e2 -> twoExp e1 e2 "" ""
    TryCatch e1 e2 -> twoExp e1 e2 "catch" "try "
    _ -> undefined
  where
    twoExp e1 e2 op pre = "(" ++ pre ++ printExp e1 ++ " " ++ op ++ " " ++ printExp e2 ++ ")"



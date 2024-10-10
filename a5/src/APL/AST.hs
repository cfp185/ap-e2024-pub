module APL.AST
  ( VName
  , Exp (..)
  , printExp
  , subExp
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


parens :: String -> String
parens x = "(" ++ x ++ ")"

printBinOp :: String -> Exp -> Exp -> String
printBinOp op x y = parens $ printExp x ++ " " ++ op ++ " " ++ printExp y

printExp :: Exp -> String
-- printExp (CstInt x) = show x
printExp (CstInt x) = if x < 0 then "(" ++ show x ++ ")" else show x
printExp (CstBool b) = if b then "true" else "false"
printExp (Add x y) = parens $ printBinOp "+" x y
printExp (Sub x y) = parens $ printBinOp "-" x y
printExp (Mul x y) = parens $ printBinOp "*" x y
printExp (Div x y) = parens $ printBinOp "/" x y
printExp (Pow x y) = parens $ printBinOp "**" x y
printExp (Eql x y) = parens $ printBinOp "==" x y
printExp (If x y z) =
  parens $
    "if "
      ++ printExp x
      ++ " then "
      ++ printExp y
      ++ " else "
      ++ printExp z
printExp (Var v) = parens v
printExp (Let v e1 e2) =
  parens $
    "let "
      ++ v
      ++ " = "
      ++ printExp e1
      ++ " in "
      ++ printExp e2
printExp (Lambda v body) =
  parens $ "\\" ++ v ++ " -> " ++ printExp body
printExp (Apply x y) =
  parens $ printExp x ++ " " ++ printExp y 
printExp (TryCatch x y) =
  parens $ "try " ++ printExp x ++ " catch " ++ printExp y

subExp :: Exp -> [Exp]
subExp e = e : case e of
  CstInt _ -> []
  CstBool _ -> []
  Add e1 e2 -> subExp e1 ++ subExp e2
  Sub e1 e2 -> subExp e1 ++ subExp e2
  Mul e1 e2 -> subExp e1 ++ subExp e2
  Div e1 e2 -> subExp e1 ++ subExp e2
  Pow e1 e2 -> subExp e1 ++ subExp e2
  Eql e1 e2 -> subExp e1 ++ subExp e2
  If e0 e1 e2 -> subExp e0 ++ subExp e1 ++ subExp e2
  Var _ -> []
  Let _ e1 e2 -> subExp e1 ++ subExp e2
  Lambda _ body -> subExp body
  Apply e1 e2 -> subExp e1 ++ subExp e2
  TryCatch e1 e2 -> subExp e1 ++ subExp e2


-- ghci error:
-- Let "pbcjrckg" (Apply (CstInt (-2)) (CstInt 2)) (Var "pbcjrckg")



-- AST.hs:
-- printExp (Let "pbcjrckg" (Apply (CstInt (-2)) (CstInt 2)) (Var "pbcjrckg"))
-- "(let pbcjrckg = ((-2) 2) in (pbcjrckg))"


-- Parser.hs:
-- parseAPL "input" "(let pbcjrckg = ((-2) 2) in (pbcjrckg))"
-- 

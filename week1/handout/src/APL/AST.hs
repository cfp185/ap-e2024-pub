module APL.AST
  (
   Exp (CstInt, Add, Sub, Mul, Div, Pow, CstBool, Eql, If, Var, Let),
   Error,
   VName
  )
where
  type Error = String
  type VName = String

  data Exp
    = CstInt Integer -- Constant Integer.
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Pow Exp Exp
    | CstBool Bool
    | Eql Exp Exp
    | If Exp Exp Exp
    | Var VName
    | Let VName Exp Exp
    deriving (Eq, Show)




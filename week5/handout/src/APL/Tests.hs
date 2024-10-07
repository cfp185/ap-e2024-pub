module APL.Tests where

import APL.AST (Exp (..), VName)
import APL.Eval (runEval, eval)
import Test.QuickCheck (
    Arbitrary (..),
    Gen,
    elements,
    listOf,
    oneof,
    sized
    )
import Data.Functor.Classes (eq1)

instance Arbitrary Exp where
    arbitrary = sized genExp
    shrink (Add x y) = x : y : [Add x' y | x' <- shrink x] ++ [Add x y' | y' <- shrink y]
    shrink (Sub x y) = x : y : [Sub x' y | x' <- shrink x] ++ [Sub x y' | y' <- shrink y]
    shrink (Mul x y) = x : y : [Mul x' y | x' <- shrink x] ++ [Mul x y' | y' <- shrink y]
    shrink (Div x y) = x : y : [Div x' y | x' <- shrink x] ++ [Div x y' | y' <- shrink y]
    shrink (Pow x y) = x : y : [Pow x' y | x' <- shrink x] ++ [Pow x y' | y' <- shrink y]
    shrink (Eql x y) = x : y : [Eql x' y | x' <- shrink x] ++ [Eql x y' | y' <- shrink y]
    shrink (If x y z) = x : y : z : [If x' y z | x' <- shrink x] ++ [If x y' z | y' <- shrink y] ++ [If x y z' | z' <- shrink z]
    shrink (Let x y z) = y : [Let x' y z| x' <- shrink x, not (null x')] ++ [Let x y' z| y' <- shrink y] ++ [Let x y z'| z' <- shrink z]
    shrink (TryCatch x y) = x : y : [TryCatch x' y| x' <- shrink x] ++ [TryCatch x y'| y' <- shrink y]
    shrink (Lambda x y) = y : [Lambda x' y | x' <- shrink x, not (null x')] ++ [Lambda x y' | y' <- shrink y]
    shrink (Apply x y) = x : y : [Apply x' y | x' <- shrink x] ++ [Apply x y' | y' <- shrink y]
    shrink (CstBool _) = []
    shrink (CstInt x) = [CstInt x' | x' <- shrink x]
    shrink (Var x) = [Var x' | x' <- shrink x, not (null x')]

prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc n1 n2 n3 = (n1 + n2) + n3 == n1 + (n2 + n3)

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval (Add e1 (Add e2 e3)))

genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)

genExp :: Int -> Gen Exp
genExp size =
  if size <= 1 
  then oneof [CstInt <$> arbitrary, CstBool <$> arbitrary, Var <$> genVar]
  else
    oneof
      [ Var <$> genVar
      , Lambda <$> genVar <*> genExp (size - 1)
      , Apply <$> genExp half <*> genExp half
      , CstInt <$> (arbitrary :: Gen Integer)
      , CstBool <$> (arbitrary :: Gen Bool)
      , Add <$> genExp half <*> genExp half
      , Sub <$> genExp half <*> genExp half   
      , Mul <$> genExp half <*> genExp half
      , Div <$> genExp half <*> genExp half
      , Pow <$> genExp half <*> genExp half
      , Eql <$> genExp half <*> genExp half
      , If <$> genExp third <*> genExp third <*> genExp third
      , Let <$> genVar <*> genExp (half - 1) <*> genExp half
      , TryCatch <$> genExp half <*> genExp half
      ]
    where
        half = (size - 1) `div` 2
        third = (size - 1) `div` 3





-- :m *APL.Eval *APL.AST *APL.Check *APL.Error + Test.QuickCheck *APL.Tests
-- :m + Test.QuickCheck APL.Tests

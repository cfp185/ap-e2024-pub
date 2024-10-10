module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName, printExp)
import APL.Parser(parseAPL, keywords)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , sized
  , frequency
  , vectorOf
  , elements
  , chooseInt
  )

instance Arbitrary Exp where
  --arbitrary = sized genExp
  arbitrary = sized (`genExp` [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

-- genVar :: Gen String
-- genVar = frequency
--   [ (70, do n <- elements [2..4]; vectorOf n (elements ['a'..'z']), if v `elem` keywords then genVar else return v)
--   , (30, do n <- chooseInt (5, 10); vectorOf n (elements ['a'..'z']), if v `elem` keywords then genVar else return v)
--   ]

genVar :: Gen String
genVar = frequency
  [ (70, do
      n <- elements [2..4]
      v <- vectorOf n (elements ['a'..'z'])
      if v `elem` keywords then genVar else return v)  -- Avoid keywords
  , (30, do
      n <- chooseInt (5, 10)
      v <- vectorOf n (elements ['a'..'z'])
      if v `elem` keywords then genVar else return v)  -- Avoid keywords
  ]

-- weights på 0 for at finde fejlene
-- i print er der ændret 2 steder og noget nogle andre steder måske men nok ikke i parser
-- printExps parenteser
-- negative tal -> absolutte værdi
-- cnstInt generes to steder de skal begge fikses


genVarFromVars :: [VName] -> Gen VName
genVarFromVars vars =
  if null vars then genVar
  else elements vars

genExp :: Int -> [VName] -> Gen Exp
genExp 0 vars = frequency 
  [ (2, CstInt . abs <$> arbitrary)
  , (2, CstBool <$> arbitrary)
  , (1, Var <$> genVarFromVars vars)
  ]                          
genExp size vars = frequency
  [ (2, CstInt . abs <$> arbitrary)
  , (2, CstBool <$> arbitrary)
  , (1, Add <$> genExp halfSize vars <*> genExp halfSize vars)
  , (1, Sub <$> genExp halfSize vars <*> genExp halfSize vars)
  , (1, Mul <$> genExp halfSize vars <*> genExp halfSize vars)
  , (2, Div <$> genExp halfSize vars <*> genExp halfSize vars)
  , (2, Pow <$> genExp halfSize vars <*> genExp halfSize vars)
  , (2, Eql <$> genExp halfSize vars <*> genExp halfSize vars)
  , (1, Var <$> genVarFromVars vars)
  , (1, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars) 
  , (8, do
      newVar <- genVar
      Let newVar <$> genExp halfSize vars <*> genExp halfSize (newVar : vars))
      -- Let newVar <$> genExp halfSize (newVar : vars) <*> genExp halfSize (newVar : vars))
  , (12, do
      newVar <- genVar
      Lambda newVar <$> genExp halfSize (newVar : vars))
  , (7, Apply <$> genExp halfSize vars <*> genExp halfSize vars)
  , (7, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars)
  ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e =
  case parseAPL "input" (printExp e) of
    Right e' -> e' == e
    _ -> False

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]


--generator MÅ ikke lave negative tal

-- :m *APL.Eval *APL.AST *APL.Check *APL.Error *APL.Tests *APL.Parser
-- :m + Test.QuickCheck APL.Tests
-- quickCheck expCoverage
-- quickCheck parsePrinted
-- quickCheck $ withMaxSuccess 1000 expCoverag

module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void, guard)
import Data.Char (isAlpha, isAlphaNum, isDigit, ord)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String

-- pExp :: Parser pExp
-- pExp = choice
--   [
--     CstInt <$> lInteger,
--     Var <$> lVName,
--     CstBool <$> lBool
--   ]

pAtom :: Parser Exp
pAtom = choice
    [ CstInt <$> lInteger,
      Var <$> lVName,
      CstBool <$> lBool,
      lString "(" *> pExp <* lString ")"
    ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp0)
        <*> (lKeyword "then" *> pExp0)
        <*> (lKeyword "else" *> pExp0),
      pAtom
    ]

pExp1 :: Parser Exp
pExp1 = pLExp >>= chain
  where
    chain x =
      choice
      [ do
          lString "*"
          y <- pLExp
          chain $ Mul x y,
        do
          lString "/"
          y <- pLExp
          chain $ Div x y,
        pure x
      ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
      [ do
          lString "+"
          y <- pExp1
          chain $ Add x y,
        do
          lString "-"
          y <- pExp1
          chain $ Sub x y,
        pure x
      ]

pExp :: Parser Exp
pExp = pExp0



lexeme :: Parser a -> Parser a
lexeme p = p <* space

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

lVar :: Parser String
lVar = lexeme $ some $ satisfy isAlpha

--lVName :: Parser VName
--lVName = lexeme $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

-- lVName :: Parser VName
-- lVName = lexeme $ do
--   c <- satisfy isAlpha
--   cs <- many $ satisfy isAlphaNum
--   cs <-
--   pure $ c:cs

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  try $ lexeme $
    choice
      [ const True <$> lKeyword "true",
        const False <$> lKeyword "false"
      ]

keywords :: [String]
keywords = ["if", "then", "else", "true", "false", "let", "in", "try", "catch", "print", "put", "get"]

lVName :: Parser String
lVName = lexeme $ do
  v <- some $ satisfy isAlpha
  if v `elem` keywords
    then fail "keyword"
    else pure v

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s


module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
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
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")",
      pPrint,
      pPut,
      pGet
    ]

fExp :: Parser Exp
fExp = do
  f <- pAtom
  fs <- many pAtom
  return $ foldl Apply f fs

lQuotes :: Parser String
lQuotes = lexeme $ do
  void $ chunk "\""
  content <- many (satisfy (/= '\"'))
  void $ chunk "\""
  return content

pPrint :: Parser Exp
pPrint = do
  lKeyword "print"
  str <- lQuotes
  e <- pExp
  return $ Print str e

pGet :: Parser Exp
pGet = do
  lKeyword "get"
  e <- pAtom
  return $ KvGet e

pPut :: Parser Exp
pPut = do
  lKeyword "put"
  e1 <- pAtom
  e2 <- pAtom
  return $ KvPut e1 e2

pLExp :: Parser Exp
pLExp =
  choice
    [
      If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      fExp,
      Lambda
        <$> (lKeyword "\\" *> lVName)
        <*> (lKeyword "->" *> pExp),
      TryCatch
        <$> (lKeyword "try" *> pExp)
        <*> (lKeyword "catch" *> pExp),
      Let
        <$> (lKeyword "let" *> lVName)
        <*> (lKeyword "=" *> pExp)
        <*> (lKeyword "in" *> pExp)
    ]


pExp2 :: Parser Exp
pExp2 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "**"
            y <- pLExp
            chain $ Pow x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp2
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp2
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

pExp0' :: Parser Exp
pExp0' = pExp0 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp0
            chain $ Eql x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0'

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

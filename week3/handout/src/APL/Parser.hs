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

pExp :: Parser Exp
pExp = choice
  [
    CstInt <$> lInteger,
    Var <$> lVName,
    CstBool <$> lBool
  ]


lexeme :: Parser a -> Parser a
lexeme p = p <* space

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x

lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

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



--if, then, else, true, false, let, in, try, catch, print, put, get

module Parser
  (
    Parser
  , Variable(..)
  , Expr(..)
  , Statement(..)
  , Sequence(..)
  , bashProgram
  , variable
  ) where

import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, anySingle, anySingleBut,
                                             empty, eof, many, notFollowedBy,
                                             satisfy, skipCount, some, try,
                                             (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, digitChar,
                                             letterChar, space1, spaceChar,
                                             string)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, skipLineComment,
                                                  space)

type Parser = Parsec Void String
-- Parsec ErrT StreamT RetT

newtype Variable = Variable String
  deriving Show

data Expr
  = VariableAccess Variable
  | SingleQuotedText String
  | DoubleQuotedText String
  deriving Show

data Statement = Assignment Variable [Expr]
  deriving Show

newtype Sequence = Sequence [Statement]
  deriving Show

customVariable :: Parser Variable
customVariable = do
  first <- letterChar <|> char '_'
  rest  <- many (alphaNumChar <|> char '_')
  return $ Variable (first:rest)

argVariable :: Parser Variable
argVariable = Variable <$> some digitChar

variable :: Parser Variable
variable = customVariable <|> argVariable

escapableChars :: [Char]
escapableChars = "$\\\""

doubleQuotedText :: Parser Expr
doubleQuotedText = do
  _   <- char '\"'
  res <- many $ try $
    notFollowedBy (char '"') >>
    ((char '\\' >> anyCharOf escapableChars) <|> anySingle)
  _   <- char '\"'
  return $ DoubleQuotedText res

singleQuotedText :: Parser Expr
singleQuotedText = do
  _   <- char '\''
  res <- many $ try (anySingleBut '\'')
  _   <- char '\''
  return $ SingleQuotedText res

anyCharOf :: [Char] -> Parser Char
anyCharOf l = satisfy (`elem` l)

plainText :: Parser Expr
plainText = do
  let controlChars = "\'\"()#$"
  res <- some $ try $
         notFollowedBy (anyCharOf controlChars <|> spaceChar) >>
         ((char '\\' >> anyCharOf escapableChars) <|> anySingle)
  return $ SingleQuotedText res

variableAccess :: Parser Expr
variableAccess = do
  _ <- char '$'
  VariableAccess <$> variable

expr :: Parser Expr
expr =  variableAccess
    <|> singleQuotedText
    <|> doubleQuotedText
    <|> plainText

assignment :: Parser Statement
assignment = do
  v <- customVariable
  _ <- string "="
  l <- many expr
  return $ Assignment v l

sc :: Parser ()
sc = L.space (space1 <|> skipCount 1 (char ';')) lineCmnt empty
  where
    lineCmnt  = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

statement :: Parser Statement
statement = lexeme assignment

bashProgram :: Parser Sequence
bashProgram = do
  res <- many statement
  eof
  return $ Sequence res

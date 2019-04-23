module Parser
  (
    Parser
  , Variable(..)
  , Expr(..)
  , Statement(..)
  , Sequence(..)
  , bashProgram
  ) where

-- import Control.Monad (void)
-- import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void (Void)
import Text.Megaparsec --(parseTest, Parsec, between, (<|>), some, many, empty, try, eof)
import Text.Megaparsec.Char --(space1, alphaNumChar, char, asciiChar, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L (space, lexeme, skipLineComment)

type Parser = Parsec Void String
-- Parsec ErrT StreamT RetT

newtype Variable = Variable String
  deriving Show

data Expr = VariableAccess Variable | SingleQuotedText String
  deriving Show

data Statement = Assignment Variable [Expr]
  deriving Show

newtype Sequence = Sequence [Statement]
  deriving Show

customVariable :: Parser Variable
customVariable = do
  first <- letterChar
  rest  <- many (alphaNumChar <|> char '_')
  return $ Variable (first:rest)

argVariable :: Parser Variable
argVariable = Variable <$> some digitChar

variable :: Parser Variable
variable = customVariable <|> argVariable

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
  let controlChars = "\'()#$"
  res <- some $ try $
         notFollowedBy (anyCharOf controlChars <|> spaceChar) >>
         ((char '\\' >> anyCharOf "$\\") <|> asciiChar)
  return $ SingleQuotedText res -- TODO?: \\ -> \

variableAccess :: Parser Expr
variableAccess = do
  _ <- char '$'
  VariableAccess <$> variable

expr :: Parser Expr
expr =  variableAccess
    <|> singleQuotedText
    <|> plainText

assignment :: Parser Statement
assignment = do
  v <- customVariable
  _ <- string "="
  l <- many expr
  return $ Assignment v l

sc :: Parser ()
sc = L.space space1 lineCmnt empty
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
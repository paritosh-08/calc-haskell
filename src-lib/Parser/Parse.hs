module Parser.Parse where

import Data.Void
import Control.Applicative hiding (many, some)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

import Calculator.Types
import Data.Ratio

type Parser a = Parsec Void String a

spaceConsumer :: Parser ()
spaceConsumer = M.space

parseOp :: Parser Op
parseOp = do
  a <- oneOf ['+','-','/','*','^']
  case a of
    '+' -> return Add
    '-' -> return Sub
    '/' -> return Div
    '*' -> return Mult
    '^' -> return Pow
    _ -> return Err

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

signedDouble :: Parser Double
signedDouble = L.signed spaceConsumer float
  where
    float = lexeme L.decimal

floatingNum :: Parser Double
floatingNum = L.signed spaceConsumer L.float

fractionalNum :: Parser Rational
fractionalNum = do
  num <- signedDouble <* spaceConsumer
  _ <- symbol "%" <* spaceConsumer
  den <- signedDouble
  pure (round num % round den)


parseNumber :: Parser Expression
parseNumber =  try 
      (Number . toRational <$> floatingNum )
  <|> (try (Number <$> fractionalNum)
  <|> (Number . toRational <$> signedDouble))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseExpression :: Parser Expression
parseExpression = 
      parseNumber
  <|> parseExp
  <|> parseSQRT
  <|> parseIFz

parseExp :: Parser Expression
parseExp = do
  _ <- symbol "("
  x <- parseNumber <|> parseExpression
  op <- parseOp <* spaceConsumer
  y <- parseNumber <|> parseExpression
  _ <- symbol ")"
  pure (Operator op x y)

parseSimExp :: Parser Expression
parseSimExp = do
  x <- parseNumber <* spaceConsumer
  op <- parseOp <* spaceConsumer
  y <- parseNumber <* spaceConsumer
  pure (Operator op x y)


parseSQRT :: Parser Expression
parseSQRT = do
  t <- chunk "sqrt"
  _ <- symbol "("
  x <- parseNumber <|> parseExpression
  _ <- symbol ")"
  pure (SQR x)

parseIFz :: Parser Expression
parseIFz = do
  _ <- chunk "ifzero"
  _ <- symbol "("
  x <- parseNumber <|> parseExpression
  _ <- symbol "?"
  y <- parseNumber <|> parseExpression
  _ <- symbol ":"
  z <- parseNumber <|> parseExpression
  _ <- symbol ")"
  pure (IfZ x y z)
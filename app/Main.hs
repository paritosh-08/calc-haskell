{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Data.Text (Text, pack)
import Data.Void
import Control.Applicative hiding (many, some)
import Control.Monad.IO.Class
import Text.Megaparsec
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type CalcMonad = ExceptT CalcException IO Double

data CalcException = NegativeSQRTError | DivideByZeroError | IncompatibleExp deriving (Show)

addF :: Double -> Double -> CalcMonad
addF a b = return (a + b)

subtractF :: Double -> Double -> CalcMonad
subtractF a b = return (a - b)

multiplyF :: Double -> Double -> CalcMonad
multiplyF a b = return (a * b)

divideF :: Double -> Double -> CalcMonad
divideF _ 0 = throwError DivideByZeroError
divideF a b = return (a / b)

mySQRT :: Double -> CalcMonad
mySQRT x =
  if x < 0
    then throwError NegativeSQRTError
    else return $ sqrt x

ifZero :: Double -> Double -> Double -> CalcMonad
ifZero 0 x _ = return x
ifZero _ _ y = return y

powF :: Double -> Double -> CalcMonad
powF x y = return $ x**y

data Op = Add | Sub | Div | Mult | Pow | Err deriving (Show)

data Expression = 
    Number Double
  | Operator Op Expression Expression -- binary
  | SQR Expression
  | IfZ Expression Expression Expression
  --- | Mult Expression Expression
  deriving (Show)

evaluate :: Expression -> CalcMonad
evaluate (Number x) = return x
evaluate (Operator op x y) = do
  x' <- evaluate x
  y' <- evaluate y
  let oper = case op of
        Add -> addF
        Sub -> subtractF
        Div -> divideF
        Pow -> powF
        Mult -> (\p _ -> do
          y'' <- catchError (evaluate y) (\e -> case e of
            DivideByZeroError -> if x' == 0 
              then do
                liftIO $ print "Error"
                return 0 
              else throwError e
            _ -> throwError e)
          multiplyF p y''
          )
        Err -> (\_ _ -> throwError IncompatibleExp)
  oper x' y'

{- evaluate (Mult x y) = do
  x' <- evaluate x
  y' <- catchError (evaluate y) (\e -> case e of
    DivideByZeroError -> if x' == 0 
      then do
        liftIO $ print "Error"
        return 0 
      else throwError e
    _ -> throwError e)
  multiplyF x' y' -}
  
evaluate (SQR x) = do
  x' <- evaluate x
  mySQRT x'
evaluate (IfZ x y z) = do
  x' <- evaluate x
  y' <- evaluate y
  z' <- evaluate z
  ifZero x' y' z'

myEXP = 
  Operator Add (Number 5) (Operator Sub (Operator Mult (Number 0) (Operator Div (Operator Add (Number 1) (Number 7)) (Number 0))) (Number 2))

-------------------------------------------------------------------

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
floatingNum = L.float

parseNumber :: Parser Expression
parseNumber = Number <$> signedDouble

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
  x <- parseNumber <|> parseExp
  _ <- symbol ")"
  pure (SQR x)

parseIFz :: Parser Expression
parseIFz = do
  _ <- chunk "ifzero"
  _ <- symbol "("
  x <- parseNumber <|> parseExp
  _ <- symbol "?"
  y <- parseNumber <|> parseExp
  _ <- symbol ":"
  z <- parseNumber <|> parseExp
  _ <- symbol ")"
  pure (IfZ x y z)


main :: IO ()
main = do
  putStrLn "Enter an expression:"
  e <- getLine
  let expr = parseMaybe parseExpression e
  case expr of 
    Nothing -> putStrLn "Something went wrong"
    Just expr' -> do
      res <- runExceptT (evaluate expr')
      print res
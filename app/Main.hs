module Main where

import Control.Monad.Except
import Text.Megaparsec
import Control.Applicative

type CalcMonad = ExceptT CalcException IO Double

data CalcException = NegativeSQRTError | DivideByZeroError deriving (Show)

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

data Op = Add | Sub | Div deriving (Show)

data Expression = 
    Number Double
  | Operator Op Expression Expression -- binary
  | SQR Expression
  | IfZ Expression Expression Expression
  | Mult Expression Expression
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
  oper x' y'

evaluate (Mult x y) = do
  x' <- evaluate x
  y' <- catchError (evaluate y) (\e -> case e of
    NegativeSQRTError -> throwError e
    DivideByZeroError -> if x' == 0 
      then do
        liftIO $ print "Error"
        return 0 
      else throwError e )
  multiplyF x' y'
  
evaluate (SQR x) = do
  x' <- evaluate x
  mySQRT x'
evaluate (IfZ x y z) = do
  x' <- evaluate x
  y' <- evaluate y
  z' <- evaluate z
  ifZero x' y' z'

myEXP = 
  Operator Add (Number 5) (Operator Sub (Mult (Number 0) (Operator Div (Operator Add (Number 1) (Number 7)) (Number 0))) (Number 2))

main :: IO ()
main = do
  print myEXP
  t <- runExceptT $ evaluate myEXP
  print t
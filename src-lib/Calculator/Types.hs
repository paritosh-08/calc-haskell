module Calculator.Types where

import Control.Monad.Except

type CalcMonad = ExceptT CalcException IO Double

data CalcException = NegativeSQRTError | DivideByZeroError | IncompatibleExp deriving (Show)

data Op = Add | Sub | Div | Mult | Pow | Err deriving (Show)

data Expression = 
    Number Double
  | Operator Op Expression Expression -- binary
  | SQR Expression
  | IfZ Expression Expression Expression
  --- | Mult Expression Expression
  deriving (Show)
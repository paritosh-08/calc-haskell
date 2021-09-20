{-# LANGUAGE DeriveGeneric #-}
module Calculator.Types where

import Control.Monad.Except
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

data CalcException = NegativeSQRTError | DivideByZeroError | IncompatibleExp deriving (Show, Eq)

data Op = Add | Sub | Div | Mult | Pow | Err deriving (Show, Generic)

instance Arbitrary Op where
  arbitrary = genericArbitrary

data Expression = 
    Number Double
  | Operator Op Expression Expression -- binary
  | SQR Expression
  | IfZ Expression Expression Expression
  --- | Mult Expression Expression
  deriving (Show, Generic)

-- pretty :: Expression -> Text
-- pretty = error "TODO"

genExpr :: Int -> Gen Expression
genExpr 1 = genNumber
genExpr n = frequency [ (1, genNumber), (3, genOp n), (3, genSqr n), (3, genIfz n) ]

genNumber :: Gen Expression
genNumber = Number <$> (arbitrary :: Gen Double)

genOp, genSqr, genIfz :: Int -> Gen Expression
genOp n = Operator <$> (arbitrary :: Gen Op) <*> genExpr (n - 1) <*> genExpr (n - 1)
genSqr n = SQR <$> genExpr (n - 1)
genIfz n = IfZ <$> genExpr (n - 1) <*> genExpr (n - 1) <*> genExpr (n - 1)


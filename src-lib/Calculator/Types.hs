{-# LANGUAGE DeriveGeneric #-}
module Calculator.Types where

import Control.Monad.Except
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

data CalcException = NegativeSQRTError | DivideByZeroError | IncompatibleExp deriving (Show, Eq)

data Op = Add | Sub | Div | Mult | Pow | Err deriving (Eq, Show, Generic)

instance Arbitrary Op where
  arbitrary = genericArbitrary

data Expression =
    Number Double
  | Operator Op Expression Expression -- binary
  | SQR Expression
  | IfZ Expression Expression Expression
  --- | Mult Expression Expression
  deriving (Eq, Show, Generic)

toStrOp :: Op -> String
toStrOp Add = "+"
toStrOp Sub = "-"
toStrOp Div = "/"
toStrOp Mult = "*"
toStrOp Pow = "^"
toStrOp Err = "This should not happen"

pretty :: Expression -> String
pretty (Number n) = show n
pretty (Operator op exp1 exp2) = "(" ++ pretty exp1 ++ toStrOp op ++ pretty exp2 ++ ")"
pretty (SQR exp1) = "sqrt(" ++ pretty exp1 ++ ")"
pretty (IfZ exp1 exp2 exp3) = "ifzero(" ++ pretty exp1 ++ "?" ++ pretty exp2 ++ ":" ++ pretty exp3 ++ ")"

genExpr :: Int -> Gen Expression
genExpr 1 = genNumber
genExpr n = frequency [ (1, genNumber), (3, genOp n), (3, genSqr n), (3, genIfz n) ]

genNumber :: Gen Expression
genNumber = Number <$> (arbitrary :: Gen Double)

genOp, genSqr, genIfz :: Int -> Gen Expression
genOp n = Operator <$> (arbitrary :: Gen Op) <*> genExpr (n - 1) <*> genExpr (n - 1)
genSqr n = SQR <$> genExpr (n - 1)
genIfz n = IfZ <$> genExpr (n - 1) <*> genExpr (n - 1) <*> genExpr (n - 1)

genNonZeroNumber :: Gen Expression
genNonZeroNumber = Number <$> nonzero

nonzero :: Gen Double
nonzero = do
    x <- chooseAny
    if x == 0
        then nonzero
        else pure x

genPositiveZeroNumber :: Gen Expression
genPositiveZeroNumber = Number <$> positiveNumber

positiveNumber :: Gen Double
positiveNumber = do
    x <- chooseAny
    if x <= 0
        then positiveNumber
        else pure x

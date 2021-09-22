{-# LANGUAGE ScopedTypeVariables #-}
module Calculator.Calculate where

import Control.Monad.Except
import Control.Monad.Writer.Strict
import Test.QuickCheck

import Calculator.Types

noErrors e = case eval e of
  Left{} -> False
  Right{} -> True

instance Arbitrary Expression where
  arbitrary = suchThat (sized genExpr) noErrors

newtype BadExpr = BadExpr { good :: Expression }
  deriving (Show)

instance Arbitrary BadExpr where
  arbitrary = suchThat (sized (\n -> BadExpr <$> genExpr n)) (not . noErrors . good)

type CalcMonad = WriterT [String] (Except CalcException) Double

eval :: Expression -> Either CalcException (Double, [String])
eval x = let
  x' :: CalcMonad = evaluate x
  x'' :: Except CalcException (Double, [String]) = runWriterT x'
  x''' = runExcept x''
  in x'''

-- round' :: Double -> Integer -> Double
-- round' num sg = (fromIntegral . round $ num * f) / f
--     where f = 10^sg

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
                tell ["Error"]
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

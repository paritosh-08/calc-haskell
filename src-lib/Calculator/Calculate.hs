module Calculator.Calculate where

import Control.Monad.Except

import Calculator.Types

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
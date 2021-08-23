module Main where

import Control.Monad.Except
import Text.Megaparsec

import Calculator.Calculate
import Calculator.Types
import Parser.Parse

myEXP = 
  Operator Add (Number 5) (Operator Sub (Operator Mult (Number 0) (Operator Div (Operator Add (Number 1) (Number 7)) (Number 0))) (Number 2))

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
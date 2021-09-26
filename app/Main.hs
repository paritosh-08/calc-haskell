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
  case runParser parseExpression "input" e of
    Left err -> putStrLn (errorBundlePretty err)
    Right expr' -> do
      case eval expr' of
        Left ce -> error (show ce)
        Right (x0, x1) ->putStrLn $ show x0 ++ " = " ++ show (fromRational x0)

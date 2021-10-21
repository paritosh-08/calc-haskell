module Main where

import Control.Monad.Except
import Text.Megaparsec

import Calculator.Calculate
import Calculator.Types
import Parser.Parse
import Data.Ratio

myEXP =
  Operator Add (Number 5) (Operator Sub (Operator Mult (Number 0) (Operator Div (Operator Add (Number 1) (Number 7)) (Number 0))) (Number 2))

eval2 expr' = case eval expr' of
        Left ce -> error (show ce)
        Right (x0, x1) -> show x0 ++ " ≈ " ++ show (fromRational x0)

eval3 expr' = case eval expr' of
        Left ce -> error (show ce)
        Right (x0, x1) -> show (fromRational x0)

main :: IO ()
main = do
  putStrLn "Enter an expression :"
  e <- getLine
  case runParser parseExpression "input" e of
    Left err -> putStrLn (errorBundlePretty err)
    Right expr' -> putStrLn $ eval2 expr'

x = SQR (Number (132329094442 % 1943931730431))
y = SQR (Number (5277604304397 % 4408623464236))
z = Operator Pow (Number (10453572296738 % 8383466229469)) (Number (12753708597271 % 6634025500147))
-- >>> eval3 (Operator Mult (Operator Pow x y) (Operator Pow x z))
-- "2.949125184606776e-2"

-- >>> eval3 (Operator Pow x (Operator Add y z))
-- "2.9491251846067757e-2"

-- >>> (eval3 x, eval3 y, eval3 z)
-- ("0.26090786194179927","1.0941249175460188","1.5284511470724913")

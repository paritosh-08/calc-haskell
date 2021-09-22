{-# LANGUAGE TypeApplications #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Calculator.Types
import Calculator.Calculate
import Control.Monad.IO.Class
import Text.Megaparsec
import Parser.Parse

-- prop_RevRev :: [Char] -> Bool
-- prop_RevRev xs = reverse (reverse xs) == xs

-- prop_RevApp :: [Char] -> [Char] -> Bool
-- prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

data InvPair = InvPair Double Double

-- InvPair a a¯¹
instance Arbitrary InvPair

-- ∀ x y. x + y = y + x
prop_add_comm :: Expression -> Expression -> Property
prop_add_comm x y = eval (Operator Add x y) === eval (Operator Add y x)

prop_mul_comm :: Expression -> Expression -> Property
prop_mul_comm x y = eval (Operator Mult x y) === eval (Operator Mult y x)

prop_add_fail_comm :: BadExpr -> BadExpr -> Property
prop_add_fail_comm (BadExpr x) (BadExpr y) = eval (Operator Add x y) === eval (Operator Add y x)
  -- where
  --   x = Number 1.0
  --   y = Number 2.0

-- TODO : handle nonzero z, failing - decimal digits don't match (after 7-8 places)
prop_div_ratio :: Expression -> Expression -> Expression -> Property
prop_div_ratio x y z = eval (Operator Div x y) === eval (Operator Div (Operator Div x z) (Operator Div y z)) -- (x/y) = ((x/z)* (y/z))
  -- z /= 0 ==> eval (Operator Div x y) === eval (Operator Div (Operator Div x z) (Operator Div y z))

-- Number (-0.2527207028827523)
-- Number (-0.9975129380016794)
-- SQR (Number 0.0)
-- Right (NaN,[]) /= Right (NaN,[])
prop_prod_of_pow :: Expression -> Expression -> Expression -> Property 
prop_prod_of_pow x y z = eval (Operator Mult (Operator Pow x y) (Operator Pow x z)) === eval (Operator Pow x (Operator Add y z)) -- Property -> (x^y)(x^z) = x^(y+z)

-- decimal digits don't match (after 7-8 places)
-- TODO: handle both negative case
prop_sqrt_prod :: Expression -> Expression -> Property
prop_sqrt_prod x y = eval (Operator Mult (SQR x) (SQR y)) === eval (SQR (Operator Mult x y)) -- Property -> (sqrt x)(sqrt y) = (sqrt x*y)

prop_pretty_parse_round_trip :: Expression -> Property
prop_pretty_parse_round_trip exp = case runParser parseExpression "inp" (pretty exp) of
  Left peb -> error "Panic"
  Right any -> any === exp

main :: IO ()
main = do
  sample (arbitrary @Double)
  -- sample (resize 1 $ arbitrary @Expression)
  -- sample (resize 2 $ arbitrary @Expression)
  quickCheckWith (stdArgs) prop_add_comm
  quickCheckWith (stdArgs) prop_mul_comm
  quickCheckWith (stdArgs) prop_div_ratio
  quickCheckWith (stdArgs) prop_prod_of_pow
  quickCheckWith (stdArgs) prop_sqrt_prod
  quickCheckWith (stdArgs) prop_pretty_parse_round_trip

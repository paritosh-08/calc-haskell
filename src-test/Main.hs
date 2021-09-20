{-# LANGUAGE TypeApplications #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Calculator.Types
import Calculator.Calculate
import Control.Monad.IO.Class

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

main :: IO ()
main = do
  sample (arbitrary @Double)
  -- sample (resize 1 $ arbitrary @Expression)
  -- sample (resize 2 $ arbitrary @Expression)
  quickCheckWith (stdArgs) prop_add_comm
  quickCheckWith (stdArgs) prop_mul_comm

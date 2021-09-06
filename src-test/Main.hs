module Main where

import Test.QuickCheck

prop_RevRev :: [Char] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevApp :: [Char] -> [Char] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

main :: IO ()
main = do
    verboseCheck prop_RevRev
    verboseCheck prop_RevApp
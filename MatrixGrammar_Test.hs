{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.Matrix
import MatrixGrammar

import Test.QuickCheck.Modifiers
  (Positive, getPositive, NonEmptyList, getNonEmpty)
import Test.QuickCheck.All (quickCheckAll)

prop_sums_mod_three :: [Int] -> Bool
prop_sums_mod_three list = result == (zero (nrows m) (ncols m)) where 
  result = evaluateMatrixExpr expr env 3
  expr = (Sum (Variable "a") (Sum (Variable "a") (Variable "a")))
  env s = m
  m = fromList 13 8 $ cycle (2:list)

-- return [] is TemplateHaskell magic to list the properties.
return []
main = $quickCheckAll

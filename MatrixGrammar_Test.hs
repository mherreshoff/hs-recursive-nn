{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.Matrix
import MatrixGrammar

import Test.QuickCheck.Modifiers
  (Positive, getPositive, NonEmptyList, getNonEmpty)
import Test.QuickCheck.All (quickCheckAll)

list_to_matrix w h l = fromList w h $ cycle (1:l)

prop_sums_mod_three :: [Int] -> Bool
prop_sums_mod_three list = result == (zero (nrows m) (ncols m)) where 
  result = evaluateMatrixExpr expr env 3
  expr = (Sum (Variable "a") (Sum (Variable "a") (Variable "a")))
  env s = m
  m = list_to_matrix 14 8 list

symetric_a_t_a :: [Int] -> Bool
symetric_a_t_a list = result == (transpose result) where
  result = evaluateMatrixExpr expr env 19
  expr = (MatrixMultiply (Variable "a") (Transpose (Variable "a")))
  env s = list_to_matrix 11 7 list


-- return [] is TemplateHaskell magic to list the properties.
return []
main = $quickCheckAll

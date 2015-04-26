{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.Matrix
import MatrixGrammar

import Test.QuickCheck.Modifiers
  (Positive, getPositive, NonEmptyList, getNonEmpty)
import Test.QuickCheck.All (quickCheckAll)

list_to_matrix w h l = fromList w h $ cycle (1:l)

-- matrixExprDimensions tests:
dummy_var_dims s = (-1, -1)

prop_var_dims :: Positive Int -> Positive Int -> Bool
prop_var_dims pw ph = result == Just (w, h) where
  result = matrixExprDimensions (Variable "a") dims
  dims "a" = (w, h)
  w = getPositive pw
  h = getPositive ph

prop_value_dims :: Positive Int -> Positive Int -> Bool
prop_value_dims pw ph = result == Just (w, h) where
  result = matrixExprDimensions (Value (zero w h)) dummy_var_dims
  w = getPositive pw
  h = getPositive ph

prop_transpose_dims :: Positive Int -> Positive Int -> Bool
prop_transpose_dims pw ph = result == Just (h, w) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = Transpose (Value (zero w h))
  w = getPositive pw
  h = getPositive ph

prop_row_sum_dims :: Positive Int -> Positive Int -> Bool
prop_row_sum_dims pw ph = result == Just (1, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = RowSum (Value (zero w h))
  w = getPositive pw
  h = getPositive ph

prop_col_sum_dims :: Positive Int -> Positive Int -> Bool
prop_col_sum_dims pw ph = result == Just (w, 1) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = ColSum (Value (zero w h))
  w = getPositive pw
  h = getPositive ph

prop_row_repeat_dims :: Positive Int -> Positive Int -> Bool
prop_row_repeat_dims pw ph = result == Just (w, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = RowRepeat w (Value (zero 1 h))
  w = getPositive pw
  h = getPositive ph

prop_col_repeat_dims :: Positive Int -> Positive Int -> Bool
prop_col_repeat_dims pw ph = result == Just (w, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = ColRepeat h (Value (zero w 1))
  w = getPositive pw
  h = getPositive ph

prop_sum_dims :: Positive Int -> Positive Int -> Bool
prop_sum_dims pw ph = result == Just (w, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = Sum (Value (zero w h)) (Value (zero w h))
  w = getPositive pw
  h = getPositive ph

prop_elementwise_multiply_dims :: Positive Int -> Positive Int -> Bool
prop_elementwise_multiply_dims pw ph = result == Just (w, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = ElementwiseMultiply (Value (zero w h)) (Value (zero w h))
  w = getPositive pw
  h = getPositive ph

prop_matrix_multiply_dims :: Positive Int -> Positive Int -> Positive Int -> Bool
prop_matrix_multiply_dims pa pb pc = result == Just (a, c) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = MatrixMultiply (Value (zero a b)) (Value (zero b c))
  a = getPositive pa
  b = getPositive pb
  c = getPositive pc

prop_matrix_multiply_bad_dims :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Bool
prop_matrix_multiply_bad_dims pa pb pc pd = result == Nothing || (b == c) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = MatrixMultiply (Value (zero a b)) (Value (zero c d))
  a = getPositive pa
  b = getPositive pb
  c = getPositive pc
  d = getPositive pd

prop_three_matrix_multiply_dims :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Bool
prop_three_matrix_multiply_dims pa pb pc pd = result == Just (a, d) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = MatrixMultiply (v a b) (MatrixMultiply (v b c) (v c d))
  v x y = Value $ zero x y
  a = getPositive pa
  b = getPositive pb
  c = getPositive pc
  d = getPositive pd

-- evaluateMatrixExpr tests:
prop_sums_mod_three :: [Int] -> Bool
prop_sums_mod_three list = result == (zero (nrows m) (ncols m)) where
  result = evaluateMatrixExpr expr env 3
  expr = (Sum (Variable "a") (Sum (Variable "a") (Variable "a")))
  env s = m
  m = list_to_matrix 14 8 list

prop_symetric_a_t_a :: [Int] -> Bool
prop_symetric_a_t_a list = result == (transpose result) where
  result = evaluateMatrixExpr expr env 19
  expr = (MatrixMultiply (Variable "a") (Transpose (Variable "a")))
  env s = list_to_matrix 11 7 list


-- return [] is TemplateHaskell magic to list the properties.
return []
main = $quickCheckAll

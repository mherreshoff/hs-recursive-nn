{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.Matrix
import MatrixGrammar

import Test.QuickCheck.Modifiers
  (Positive(Positive), NonEmptyList, getNonEmpty)
import Test.QuickCheck.All (quickCheckAll)

-- matrixExprDimensions tests:
dummy_var_dims s = (-1, -1)

prop_var_dims :: Positive Int -> Positive Int -> Bool
prop_var_dims (Positive w) (Positive h) = result == Just (w, h) where
  result = matrixExprDimensions (Variable "a") dims
  dims "a" = (w, h)

prop_value_dims :: Positive Int -> Positive Int -> Bool
prop_value_dims (Positive w) (Positive h) = result == Just (w, h) where
  result = matrixExprDimensions (Value (zero w h)) dummy_var_dims

prop_transpose_dims :: Positive Int -> Positive Int -> Bool
prop_transpose_dims (Positive w) (Positive h) = result == Just (h, w) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = Transpose (Value (zero w h))

prop_row_sum_dims :: Positive Int -> Positive Int -> Bool
prop_row_sum_dims (Positive w) (Positive h) = result == Just (1, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = RowSum (Value (zero w h))

prop_col_sum_dims :: Positive Int -> Positive Int -> Bool
prop_col_sum_dims (Positive w) (Positive h) = result == Just (w, 1) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = ColSum (Value (zero w h))

prop_row_repeat_dims :: Positive Int -> Positive Int -> Bool
prop_row_repeat_dims (Positive w) (Positive h) = result == Just (w, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = RowRepeat w (Value (zero 1 h))

prop_col_repeat_dims :: Positive Int -> Positive Int -> Bool
prop_col_repeat_dims (Positive w) (Positive h) = result == Just (w, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = ColRepeat h (Value (zero w 1))

prop_sum_dims :: Positive Int -> Positive Int -> Bool
prop_sum_dims (Positive w) (Positive h) = result == Just (w, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = Sum (Value (zero w h)) (Value (zero w h))

prop_elementwise_multiply_dims :: Positive Int -> Positive Int -> Bool
prop_elementwise_multiply_dims (Positive w) (Positive h) = result == Just (w, h) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = ElementwiseMultiply (Value (zero w h)) (Value (zero w h))

prop_matrix_multiply_dims :: Positive Int -> Positive Int -> Positive Int -> Bool
prop_matrix_multiply_dims (Positive a) (Positive b) (Positive c) = result == Just (a, c) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = MatrixMultiply (Value (zero a b)) (Value (zero b c))

prop_matrix_multiply_bad_dims :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Bool
prop_matrix_multiply_bad_dims (Positive a) (Positive b) (Positive c) (Positive d) = result == Nothing || (b == c) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = MatrixMultiply (Value (zero a b)) (Value (zero c d))

prop_three_matrix_multiply_dims :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Bool
prop_three_matrix_multiply_dims (Positive a) (Positive b) (Positive c) (Positive d) = result == Just (a, d) where
  result = matrixExprDimensions expr dummy_var_dims
  expr = MatrixMultiply (v a b) (MatrixMultiply (v b c) (v c d))
  v x y = Value $ zero x y

-- evaluateMatrixExpr tests:
list_to_matrix :: Int -> Int -> [Int] -> Matrix Int
list_to_matrix w h l = fromList w h $ cycle (1:l)

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

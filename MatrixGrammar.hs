module MatrixGrammar (
  MatrixExpr(Variable,Value,
             Transpose,RowSum,ColSum,RowRepeat,ColRepeat,
             Sum,MatrixMultiply,ElementwiseMultiply),
  matrixExprDimensions, evaluateMatrixExpr) where
import Control.Applicative
import Data.Matrix

data MatrixExpr =
    Variable String
  | Value (Matrix Int)
  | Transpose MatrixExpr
  | RowSum MatrixExpr
  | ColSum MatrixExpr
  | RowRepeat Int MatrixExpr
  | ColRepeat Int MatrixExpr
  | Sum MatrixExpr MatrixExpr
  | MatrixMultiply MatrixExpr MatrixExpr
  | ElementwiseMultiply MatrixExpr MatrixExpr

-- matrixExprDimensions takes a matrix expression and dimensions for all of its
-- variables and returns:
--   Nothing if the expression is invalid
--   Just (rows, cols) if the expression is valid
matrixExprDimensions :: MatrixExpr -> (String -> (Int, Int)) -> Maybe (Int, Int)
matrixExprDimensions expr varDims = iter expr where
  iter (Variable v) = Just $ varDims v
  iter (Value m) = Just $ (nrows m, ncols m)
  iter (Transpose x) = (\(r, c) -> (c, r)) <$> iter x
  iter (RowSum x) = (\(r, c) -> (1, c)) <$> iter x
  iter (ColSum x) = (\(r, c) -> (r, 1)) <$> iter x
  iter (RowRepeat rr x) = iter x >>= (\(r, c) ->
      if r == 1 then Just (rr, c) else Nothing)
  iter (ColRepeat cc x) = iter x >>= (\(r, c) ->
      if c == 1 then Just (r, cc) else Nothing)
  iter (Sum x y) = sameDimensions (iter x) (iter y)
  iter (ElementwiseMultiply x y) = sameDimensions (iter x) (iter y)
  iter (MatrixMultiply x y) = f (iter x) (iter y) where
    f (Just (a,b)) (Just (b', c)) | b == b' = Just (a,c)
    f _ _ = Nothing
  sameDimensions (Just (a,b)) (Just (a',b')) | a == a', b == b' = Just (a,b)
  sameDimensions _ _ = Nothing

-- Evaluate matrix expression mod p
evaluateMatrixExpr :: MatrixExpr -> (String -> Matrix Int) -> Int -> Matrix Int
evaluateMatrixExpr expr env p = f expr where
  f expr = fmap (`mod`p) (g expr)
  g (Variable v) = env v
  g (Value m) = m
  g (Transpose e) = transpose $ f e
  g (RowSum e) = let m = f e in multStd (matrix 1 (nrows m) (const 1)) m
  g (ColSum e) = let m = f e in multStd m (matrix (ncols m) 1 (const 1))
  g (RowRepeat rr e) = let m = f e in matrix rr (ncols m) (\(r, c) -> m!(1, c))
  g (ColRepeat cc e) = let m = f e in matrix (nrows m) cc (\(r, c) -> m!(r, 1))
  g (Sum e1 e2) = elementwise (+) (f e1) (f e2)
  g (ElementwiseMultiply e1 e2) = elementwise (*) (f e1) (f e2)
  g (MatrixMultiply e1 e2) = multStd (f e1) (f e2)

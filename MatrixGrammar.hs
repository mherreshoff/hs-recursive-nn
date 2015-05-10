module MatrixGrammar (
  MatrixExpr(Variable,Value,
             Transpose,RowSum,ColSum,RowRepeat,ColRepeat,
             Sum,MatrixMultiply,ElementwiseMultiply),
  matrixExprDepth, matrixSubExprs, drawMatrixExpr,
  matrixExprDimensions, evaluateMatrixExpr,
  validOneVariableExpr,
  bucketExpressionsByEvaluations,
  ) where
import Control.Applicative
import Data.Function
import qualified Data.Matrix as M
import Data.Maybe
import Data.Tree
import qualified Data.List as List
import qualified GHC.Exts as Exts

instance Ord a => Ord (M.Matrix a) where
  compare m m' | (M.nrows m) /= (M.nrows m') = compare (M.nrows m) (M.nrows m')
  compare m m' | (M.ncols m) /= (M.ncols m') = compare (M.ncols m) (M.ncols m')
  compare m m' = compare (elements m) (elements m') where
    elements m = [m M.!(r,c) | r <- [1..(M.nrows m)], c <- [1..(M.ncols m)]]

data MatrixExpr =
    Variable String
  | Value (M.Matrix Int)
  | Transpose MatrixExpr
  | RowSum MatrixExpr
  | ColSum MatrixExpr
  | RowRepeat Int MatrixExpr
  | ColRepeat Int MatrixExpr
  | Sum MatrixExpr MatrixExpr
  | MatrixMultiply MatrixExpr MatrixExpr
  | ElementwiseMultiply MatrixExpr MatrixExpr
  deriving (Show,Ord,Eq)

-- Conversion to Data.Tree and related operations
matrixExprTreeView :: MatrixExpr -> Tree MatrixExpr
matrixExprTreeView = unfoldTree (\x -> (x, children x)) where
  children (Variable v) = []
  children (Value v) = []
  children (Transpose e) = [e]
  children (RowSum e) = [e]
  children (ColSum e) = [e]
  children (RowRepeat rr e) = [e]
  children (ColRepeat cc e) = [e]
  children (Sum e1 e2) = [e1, e2]
  children (ElementwiseMultiply e1 e2) = [e1, e2]
  children (MatrixMultiply e1 e2) = [e1, e2]

matrixExprDepth :: MatrixExpr -> Int
matrixExprDepth = length . levels . matrixExprTreeView

matrixSubExprs :: MatrixExpr -> [MatrixExpr]
matrixSubExprs = flatten . matrixExprTreeView

-- drawMatrixExpr is for printing out pretty pictures of expression trees
drawMatrixExpr :: MatrixExpr -> String
drawMatrixExpr = drawTree . (fmap f) . matrixExprTreeView where
  f (Variable v) = v
  f (Value m) = show m
  f (Transpose _) = "T"
  f (RowSum _) = "rowsum"
  f (ColSum _) = "colsum"
  f (RowRepeat rr _) = "rowrep:" ++ (show rr)
  f (ColRepeat cc _) = "colrep:" ++ (show cc)
  f (Sum _ _) = "sum"
  f (ElementwiseMultiply _ _) = "elt-prod"
  f (MatrixMultiply _ _) = "prod"

-- matrixExprDimensions takes a matrix expression and dimensions for all of its
-- variables and returns:
--   Nothing if the expression is invalid
--   Just (rows, cols) if the expression is valid
matrixExprDimensions :: MatrixExpr -> (String -> (Int, Int)) -> Maybe (Int, Int)
matrixExprDimensions expr varDims = iter expr where
  iter (Variable v) = Just $ varDims v
  iter (Value m) = Just $ (M.nrows m, M.ncols m)
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

evaluateMatrixExpr :: (String -> M.Matrix Int) -> Int -> MatrixExpr -> M.Matrix Int
evaluateMatrixExpr env p expr = f expr where
  f expr = fmap (`mod`p) (g expr)
  g (Variable v) = env v
  g (Value m) = m
  g (Transpose e) = M.transpose $ f e
  g (RowSum e) = let m = f e in M.multStd (M.matrix 1 (M.nrows m) (const 1)) m
  g (ColSum e) = let m = f e in M.multStd m (M.matrix (M.ncols m) 1 (const 1))
  g (RowRepeat rr e) = let m = f e in M.matrix rr (M.ncols m) (\(r, c) -> m M.!(1, c))
  g (ColRepeat cc e) = let m = f e in M.matrix (M.nrows m) cc (\(r, c) -> m M.!(r, 1))
  g (Sum e1 e2) = M.elementwise (+) (f e1) (f e2)
  g (ElementwiseMultiply e1 e2) = M.elementwise (*) (f e1) (f e2)
  g (MatrixMultiply e1 e2) = M.multStd (f e1) (f e2)


-- Valid expressions
validOneVariableExpr :: (Int, Int) -> Int -> [MatrixExpr]
validOneVariableExpr _ 1 = [(Variable "a")]
validOneVariableExpr dims n | n > 1 = result where
  result = map head $ List.group $ List.sort $ unsorted_result
  unsorted_result = previous ++ (filter goodExpr $ unary_exprs ++ binary_exprs)
  previous = validOneVariableExpr dims (n-1)
  unary_ops = [Transpose, RowSum, ColSum,
      RowRepeat 2, RowRepeat 3, ColRepeat 2, ColRepeat 3]
  varDims "a" = dims
  goodExpr e = (matrixExprDepth e <= n) && isJust (matrixExprDimensions e varDims)
  binary_ops = [Sum, ElementwiseMultiply, MatrixMultiply]
  unary_exprs = [op x | op <- unary_ops, x <- previous]
  binary_exprs = [op x y | op <- binary_ops, x <- previous, y <- previous]

-- Bucket Expressions by Evaluations
bucketExpressionsByEvaluations :: [(String -> M.Matrix Int)] -> Int -> [MatrixExpr] -> [[MatrixExpr]]

bucketExpressionsByEvaluations envs p exprs = result where
  evaluators :: [MatrixExpr -> M.Matrix Int]
  evaluators = [evaluateMatrixExpr env p | env <- envs]
  evals :: MatrixExpr -> ([M.Matrix Int], MatrixExpr)
  evals expr = (($expr) <$> evaluators, expr)
  result :: [[MatrixExpr]]
  result = map (map snd) $ Exts.groupWith fst $ map evals exprs

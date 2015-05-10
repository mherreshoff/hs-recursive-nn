import MatrixGrammar

import Control.Applicative
import Data.Function
import Data.Matrix
import Data.Maybe
import Data.Tree
import qualified Data.List as List
import qualified GHC.Exts as Exts


i2m :: Int -> Matrix Int
i2m x = matrix 1 1 (const x)

a :: MatrixExpr
a = Variable "a"

repPlus :: MatrixExpr -> Int -> MatrixExpr
repPlus x 1 = x
repPlus x n | n > 1 = Sum x (repPlus x (n-1))

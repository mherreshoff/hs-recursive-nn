-- This file is to be used as the entry point for ghci for performing little experiments.

import Control.Applicative
import Data.Function
import qualified Data.Matrix as M
import Data.Maybe
import Data.Tree
import qualified Data.List as List
import qualified GHC.Exts as Exts

import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

import MatrixGrammar

infixl 9 #
ds # cs = listArray ds cs :: Array Double

sh x = putStr . formatFixed 2 $ x


i2m :: Int -> M.Matrix Int
i2m x = matrix 1 1 (const x)

a :: MatrixExpr
a = Variable "a"

repPlus :: MatrixExpr -> Int -> MatrixExpr
repPlus x n = (iterate (Sum x) x) !! (n-1)

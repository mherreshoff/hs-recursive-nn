{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Test.QuickCheck.All (quickCheckAll)

import Data.Tree
import RecursiveNeuralNet
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

-- Here's a mini-language
x = Node 0 []
y = Node 1 []
neg a = Node 2 [a]
ewProd a b = Node 3 [a, b]

indicator f = (\x -> if x then 1.0 else 0.0).f
-- here's an example array definitions
exampleParams :: NetParameters
-- x and y:
exampleParams 0 = listArray [5] [1,-1,5,-6,3]
exampleParams 1 = listArray [5] [3,-2,-6,1,-2]

-- neg:
exampleParams 2 = mkFun [5,5] $ indicator (\[x,y] -> x+y == 4)

-- ewProd:
exampleParams 3 = mkFun [5,5,5] $ indicator (\[x,y,z] -> x == y && y == z)

exampleEval a = rootLabel $ evalNet id exampleParams a

-- Tests:
prop_double_flip_is_id0 :: Bool

prop_double_flip_is_id0 =
  exampleParams 0 == exampleEval (neg (neg x))

prop_double_flip_is_id1 :: Bool
prop_double_flip_is_id1 =
  exampleParams 1 == exampleEval (neg (neg y))

prop_ew_prod_works :: Bool
prop_ew_prod_works =
  listArray [5] [3, 2, -30, -6, -6] == exampleEval (ewProd x y)

-- return [] is TemplateHaskell magic to list the properties.
return []
main = $quickCheckAll

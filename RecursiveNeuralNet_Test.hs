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

-- here's an example array definitions
exampleParams :: NetParameters
-- x and y:
exampleParams 0 = listArray [5] [1,-1,5,-6,3]
exampleParams 1 = listArray [5] [3,-2,-6,1,-2]

-- neg:
exampleParams 2 = mkFun [5,5] (\[x,y] -> if x+y == 4 then 1.0 else 0.0)

exampleEval a = evalNet id exampleParams a

-- Tests:
prop_double_flip_is_id0 :: Bool

prop_double_flip_is_id0 =
  exampleEval x == exampleEval (neg (neg x))

prop_double_flip_is_id1 :: Bool
prop_double_flip_is_id1 =
  exampleEval y == exampleEval (neg (neg y))

-- return [] is TemplateHaskell magic to list the properties.
return []
main = $quickCheckAll

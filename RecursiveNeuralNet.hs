module RecursiveNeuralNet (
  evalNet
  ) where

import Data.Tree
import qualified Data.Vector.Storable as V
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import TreeUtil

type NodeType = Int;

type NetParameters = NodeType -> Array Double;

elementwiseOp :: (Coord a, Coord b) => (a -> b) -> NArray i a -> NArray i b
elementwiseOp f = mapArray (V.map f)


-- NodeType has to map to an array of dimension one higher than the arity of the node.
evalNet :: NetParameters -> Tree NodeType -> Tree (Array Double)
evalNet params tree = evaluationTree eval tree where
  eval :: NodeType -> [Array Double] -> Array Double
  eval id children = elementwiseOp sigmoid $ product (params id) children
  sigmoid :: Double -> Double
  sigmoid x = 1.0 / (1.0 + exp (0.0-x))
  product :: Array Double -> [Array Double] -> Array Double
  product arr vecs = (!"1") $ foldl (*) (arr!letters) $
     zipWith (!) vecs $ map (:[]) letters
  letters :: [Char]
  letters = ['a'..'z']

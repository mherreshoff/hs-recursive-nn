module RecursiveNeuralNet (
  sigmoid,
  elementwiseOp,
  NodeType,
  NetParameters,
  evalNet,
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

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (0.0-x))

-- NodeType has to map to an array of dimension one higher than the arity of the node.
-- squash ought to be a function that maps reals into the [0,1] range (e.g. sigmoid)
evalNet :: (Double -> Double) -> NetParameters -> Tree NodeType -> Tree (Array Double)
evalNet squash params tree = evaluationTree eval tree where
  eval :: NodeType -> [Array Double] -> Array Double
  eval id children = elementwiseOp squash $ product (params id) children
  product :: Array Double -> [Array Double] -> Array Double
  product arr vecs = (!"1") $ foldl (*) (arr!letters) $
     zipWith (!) vecs $ map (:[]) letters
  letters :: [Char]
  letters = ['a'..'z']


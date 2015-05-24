import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import TreeUtil

type NodeType = Int;

type NetParameters = NodeType -> Array Double;

-- TODO(mherreshoff): get this to work.
-- NodeType has to map to an array of dimension one higher than the arity of the node.
-- evalNet :: NetParameters -> Tree NodeType -> Tree (Array Double)
-- evalNet params = evaluationTree (sigmoid.product.params) where
--   letters = ['a'..'z']
--   product :: Array Double -> [Array Double] -> Array Double
--   product arr vecs = (!"1") $ foldl (*) (arr!letters) $
--      zipWith (!) vecs $ map (:[]) letters
--   sigmoid :: Array Double -> Array Double
--   sigmoid = fmap (\x -> 1.0 / (1.0 + exp (0.0-x)))

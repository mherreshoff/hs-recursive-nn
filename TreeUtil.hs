module TreeUtil (
  metaTree, evaluationTree, treeMessageFlow
  ) where
import Data.Tree
import Data.Traversable
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

type NodeType = Int;

type NetParameters = NodeType -> Array Double;

metaTree :: Tree a -> Tree (Tree a)
metaTree = unfoldTree (\t -> (t, (subForest t)))

evaluationTree :: (a -> [b] -> b) -> Tree a -> Tree b
evaluationTree f = iter where
  iter tree = Node result sub_results where
    result = f (rootLabel tree) (map rootLabel sub_results)
    sub_results = map iter (subForest tree)

treeMessageFlow :: (b -> [a] -> ([b], c)) -> b -> Tree a -> Tree c
treeMessageFlow f = iter where
  iter message tree = Node result sub_results where
    (sub_messages, result) = f message (map rootLabel (subForest tree))
    sub_results = zipWith iter sub_messages (subForest tree)



module TreeUtil (
  zipTrees, metaTree, evaluationTree, treeMessageFlow
  ) where
import Data.Tree
import Data.Traversable

zipTrees :: Tree a -> Tree b -> Tree (a, b)
zipTrees (Node a as) (Node b bs) = Node (a, b) (zipWith zipTrees as bs)

metaTree :: Tree a -> Tree (Tree a)
metaTree = unfoldTree (\t -> (t, (subForest t)))

evaluationTree :: (a -> [b] -> b) -> Tree a -> Tree b
evaluationTree f = iter where
  iter tree = Node result sub_results where
    result = f (rootLabel tree) (map rootLabel sub_results)
    sub_results = map iter (subForest tree)

-- treeMessageFlow types:
-- 'a' - the type of the input tree
-- 'b' - the type of the message
-- 'c' - the type of the output
treeMessageFlow :: (b -> a -> (b, c)) -> b -> Tree a -> Tree c
treeMessageFlow flow message tree = unfoldTree builder (message, tree) where
  builder (m, (Node r xs)) = (r', [(m', x) | x <- xs]) where (m', r') = flow m r


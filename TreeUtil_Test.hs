{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Control.Applicative
import Data.Tree
import TreeUtil

-- evaluationTree tests

uniformSample :: (Arbitrary a) => [Gen a] -> Gen a
uniformSample gs = frequency $ zip [1..] gs

arbitraryTreeWithDepth :: (Arbitrary a) => Int -> Gen (Tree a)
arbitraryTreeWithDepth 1 = fmap (\x -> Node x []) arbitrary
arbitraryTreeWithDepth n | n > 1 = uniformSample [one_child, two_children] where
  one_child = (\x c -> Node x [c]) <$> arbitrary <*> child
  two_children = (\x c c' -> Node x [c, c']) <$> arbitrary <*> child <*> child
  child = arbitraryTreeWithDepth (n-1)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = frequency $ zip [1..] (map arbitraryTreeWithDepth [1..5])
  shrink _ = []

-- printArbTree prints out one arbitrary tree of ints to give you an idea of what samples
-- look like
printArbTree :: IO ()
printArbTree = do
  tree <- generate (arbitrary :: Gen (Tree Int))
  putStrLn $ drawTree $ fmap show tree

-- Tests for metaTree

prop_meta_tree_labels_make_tree :: Tree Int -> Bool
prop_meta_tree_labels_make_tree tree =
  tree == fmap rootLabel (metaTree tree)

-- Tests for evaluationTree
prop_node_count_evaluator :: Tree Int -> Bool
prop_node_count_evaluator tree =
  rootLabel (evaluationTree (\x ys -> 1 + (sum ys)) tree) == length (flatten tree)

prop_node_sum_evaluator :: Tree Int -> Bool
prop_node_sum_evaluator tree =
  rootLabel (evaluationTree (\x ys -> x + (sum ys)) tree) == sum (flatten tree)

prop_node_sum_evaluator_sub_eval tree =
  sumTree tree == fmap (rootLabel.sumTree) (metaTree tree) where
  sumTree t = evaluationTree (\x ys -> x + (sum ys)) t

-- Tests for treeMessageFlow
pathSum :: Tree Int -> Tree Int
pathSum = treeMessageFlow (\s n -> (s+n, s+n)) 0

deltaFromParent :: Tree Int -> Tree Int
deltaFromParent = treeMessageFlow (\p n -> (n, n-p)) 0

prop_sum_then_delta_is_id :: Tree Int -> Bool
prop_sum_then_delta_is_id tree =
  tree == deltaFromParent (pathSum tree)


-- return [] is TemplateHaskell magic to list the properties.
return []
main = $quickCheckAll

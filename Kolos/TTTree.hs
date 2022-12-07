------------------------------------------------------
-- Module: TTTree                                   --
-- Author: Maciej Bazela (261743)                   --
--                                                  --
-- This module contains a data type for (2,3)-trees --
-- and some functions to work with them.            --
------------------------------------------------------
module TTTree
( TTTree(..)
, expand
, trav
, depth
) where

-------------------------------------
-- Imports from standard libraries --
-------------------------------------

import Control.Monad.Writer ( MonadWriter(tell), Writer )
import Data.Semigroup (Sum(..), Max(..))

---------------------------
-- (2,3)-tree data type  --
-- TTTree = TwoThreeTree --
---------------------------

-- | (2,3)-tree data type
data TTTree a = Leaf a
                | Binary a (TTTree a) (TTTree a)
                | Ternary a (TTTree a) (TTTree a) (TTTree a)
                deriving (Eq)

----------------------
-- (2,3)-tree print --
----------------------

-- | (2,3)-tree print
--
-- Examples:
-- >>> show $ Leaf 1
-- "1"
-- >>> show $ Binary 1 (Leaf 2) (Leaf 3)
-- "(1) -> {2; 3}"
-- >>> show $ Ternary 1 (Binary 2 (Leaf 3) (Leaf 4)) (Leaf 5) (Leaf 6)
-- "(1) -> {(2) -> {3; 4}; 5; 6}"
instance (Show a) => Show (TTTree a) where
    show (Leaf a) = show a
    show (Binary a b c) = "(" ++ show a ++ ") -> {" ++ show b ++ "; " ++ show c ++ "}"
    show (Ternary a b c d) = "(" ++ show a ++ ") -> {" ++ show b ++ "; " ++ show c ++ "; " ++ show d ++ "}"

-----------------------------
-- (2,3)-tree as a Functor --
-----------------------------

-- | (2,3)-tree as a Functor
--
-- Examples:
-- >>> (+1) <$> Leaf 1
-- Leaf 2
-- >>> (+1) <$> Binary 1 (Leaf 2) (Leaf 3)
-- Binary 2 (Leaf 3) (Leaf 4)
-- >>> (+1) <$> Ternary 1 (Binary 2 (Leaf 3) (Leaf 4)) (Leaf 5) (Leaf 6)
-- Ternary 2 (Binary 3 (Leaf 4) (Leaf 5)) (Leaf 6) (Leaf 7)
instance Functor TTTree where
    fmap f (Leaf val) = Leaf (f val)
    fmap f (Binary val left right) = Binary (f val) (fmap f left) (fmap f right)
    fmap f (Ternary val left middle right) = Ternary (f val) (fmap f left) (fmap f middle) (fmap f right)


-----------------------
-- (2,3)-tree expand --
-----------------------

-- | `expand` binary nodes of (2,3)-tree into ternary nodes.
-- * New node with `newVal` value is inserted as a middle child of the ternary node.
-- * If the node is a leaf, it is not expanded.
-- * If the node is a ternary node, it is not expanded.
expand :: TTTree a -> a -> TTTree a
expand (Leaf val) _ = Leaf val
expand (Binary val left right) newVal  = 
    Ternary val (expand left newVal) (Leaf newVal) (expand right newVal)
expand (Ternary val left middle right) newVal = 
    Ternary val (expand left newVal) (expand middle newVal) (expand right newVal)


------------------------------------
-- (2,3)-tree traverse statistics --
------------------------------------

-- | `trav` traverses (2,3)-tree and returns the same tree with statistics of traversed nodes.
-- * Statistics are returned as a `Writer` monad of three (`Sum` `Int`) values: 
-- * (number of leaves, number of  of binary nodes, number of of ternary nodes).
--
-- Usage:
-- * `fst` $ `execWriter` $ `trav` tree - returns the same tree.
-- * `snd` $ `execWriter` $ `trav` tree - returns statistics of traversed nodes.
trav :: TTTree a -> Writer (Sum Int, Sum Int, Sum Int) (TTTree a)
trav tree@(Leaf _) =
    do
      tell (Sum 1, Sum 0, Sum 0)
      return tree
trav tree@(Binary _ left right) =
    do
      tell (Sum 0, Sum 1, Sum 0)
      trav left
      trav right
      return tree
trav tree@(Ternary _ left middle right) =
    do
      tell (Sum 0, Sum 0, Sum 1)
      trav left
      trav middle
      trav right
      return tree

----------------------
-- (2,3)-tree depth --
----------------------

-- | `depth'` is a helper function which calculates the depth of a (2,3)-tree.
-- * It remembers current depth (`acc`) for given child and
--   writes the maximum depth of all children, when reaching a leaf node.
-- * Depth of a leaf is 0.
-- * Depth of a binary node is 1 + maximum depth of its children.
-- * Depth of a ternary node is 1 + maximum depth of its children.
-- * Writer monad is used to remember the maximum depth of all children.
depth' :: TTTree a -> Int -> Writer (Max Int) (TTTree a)
depth' tree@(Leaf _) acc = 
    do
      tell (Max acc)
      return tree
depth' tree@(Binary _ left right) acc = 
    do
      depth' left (acc + 1)
      depth' right (acc + 1)
      return tree

depth' tree@(Ternary _ left middle right) acc = 
    do
      depth' left (acc + 1)
      depth' middle (acc + 1)
      depth' right (acc + 1)      
      return tree


-- | `depth` returns the depth of the (2,3)-tree.
-- * Depth of a leaf is 0.
-- * Depth of a binary node is 1 + maximum depth of its children.
-- * Depth of a ternary node is 1 + maximum depth of its children.
-- * Writer monad is used to remember the maximum depth of all children.
--
-- Usage:
-- * `fst` $ `execWriter` $ `depth` tree - returns the same tree.
-- * `snd` $ `execWriter` $ `depth` tree - returns the depth of the tree (as `Max` `Int`).
depth :: TTTree a -> Writer (Max Int) (TTTree a)
depth tree = depth' tree 0


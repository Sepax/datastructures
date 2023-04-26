module SkewHeap (Skew(..), merge, insert, getMin, delMin, single, emptySkew, toSkew) where

import Data.List (foldl', unfoldr)
import Data.Maybe

-- data type for our Skewheap, Basic tree structure 
data Skew a = Leaf | Node a (Skew a) (Skew a) deriving Show

-- creates a empty skew
emptySkew :: Skew a
emptySkew = Leaf

-- creates a skew with a single node
single :: Ord a => a -> Skew a
single x = Node x Leaf Leaf

-- fundemental function that gives our tree structure SkewHeap properites 
merge :: Ord a => Skew a -> Skew a -> Skew a
merge t1 Leaf = t1
merge Leaf t2 = t2
merge t1@(Node e1 l1 r1) t2@(Node e2 l2 r2)
    | e1 <= e2 = Node e1 (merge t2 r1) l1
    | otherwise = Node e2 (merge t1 r2) l2

-- insert element into Skew
insert :: Ord a => a -> Skew a -> Skew a
insert x heap = merge (single x) heap

-- find minimum element from Skew
getMin :: Ord a => Skew a -> Maybe a
getMin Leaf = Nothing
getMin (Node x l r) = Just x

-- extract minimum element from Skew
delMin :: Ord a => Skew a -> Skew a
delMin Leaf = Leaf
delMin (Node x l r) = (merge l r)

-- create a Skew from a list
toSkew :: Ord a => [a] -> Skew a
toSkew = foldl' (flip insert) emptySkew

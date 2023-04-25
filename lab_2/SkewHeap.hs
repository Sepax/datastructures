module SkewHeap (Skew(..), mergeSkew, deleteSkew, insertSkew, single, emptySkew, isEmpty) where

-- data type for our Skewheap, Basic tree structure
data Skew a = Empty | Node a (Skew a) (Skew a) deriving Show



isEmpty :: Skew a -> Bool
isEmpty skew = case skew of 
    Empty -> True
    _     -> False

-- creates a empty skew
emptySkew :: Skew a
emptySkew = Empty

-- creates a skew with a single node of type a
single :: a -> Skew a
single x = Node x Empty Empty

-- fundemental function that gives our tree structure SkewHeap properites 
mergeSkew :: Ord a => Skew a -> Skew a -> Skew a
mergeSkew (Node elem (left) (right)) Empty = (Node elem (left) (right))
mergeSkew Empty (Node elem (left) (right)) = (Node elem (left) (right))
mergeSkew Empty Empty                      = Empty
mergeSkew (Node elem1 (l1) (r1)) (Node elem2 (l2) (r2)) 
    | elem1 <= elem2 = Node elem1 (mergeSkew r1 (Node elem2 (l2) (r2))) (l1)
    | otherwise     = Node elem2 (mergeSkew r2 (Node elem1 (l1) (r2))) (l2)

-- delete element from Skew
deleteSkew ::  Ord a => a -> Skew a -> Skew a
deleteSkew _ Empty = Empty
deleteSkew x (Node elem (left) (right))
    | x < elem     = Node elem (deleteSkew x right) left
    | x > elem     = Node elem left (deleteSkew x right)
    | otherwise    = mergeSkew left right

-- inserts element into skew
insertSkew :: Ord a => a -> Skew a -> Skew a
insertSkew newElem Empty = Node newElem Empty Empty
insertSkew newElem (Node elem (left) (right))
    | newElem > elem  = (Node elem (insertSkew newElem right) (left))
    | otherwise       = (Node newElem (Node elem (left) (right)) Empty)

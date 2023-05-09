{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node a (AATree a) (AATree a) Int
  deriving (Eq, Show, Read)

--Gives empty tree
emptyTree :: AATree a
emptyTree = Empty

-- Tells us if a value is in the AATree and returns a Maybe

get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get value ((Node x left right _))
  | value > x = get value left
  | value < x = get value right
  | value == x = Just x
  | otherwise = Nothing



-- Helpers (Kinda) for insert!
split :: AATree a -> AATree a
split t@(Node x a (Node y b (Node z c d kright) k2) k1)
  | k1 == k2 && k1 == kright = (Node y (Node x a b k1) (Node z c d k1) (k1+1))
  | otherwise = t
split t = t

skew  :: AATree a -> AATree a
skew t@(Node y (Node x a b level') c level) 
  | level' == level = (Node x a (Node y b c level) level)
  | otherwise = t 
skew t = t



--inserts given value into AATree, if value already exist in tree -> return input tree unchanged
insert :: Ord a => a -> AATree a -> AATree a
insert e Empty = Node e Empty Empty 1
insert e t@(Node n l r lvl) = case compare e n of
  EQ -> t
  LT -> go $ Node n (insert e l) r lvl
  GT -> go $ Node n l (insert e r) lvl
  where
    go = split . skew

inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node x Empty Empty _) = [x]
inorder (Node x left right _) = inorder left ++ [x] ++ inorder right

size :: AATree a -> Int
size Empty = 0
size (Node _ left right _) = size left + size right + 1

height :: AATree a -> Int
height Empty = -1
height (Node _ left right _ ) 
  | height left > height right = height left + 1
  | otherwise = height right + 1  




--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted []     = True
isSorted [x] = True
isSorted (x:xs) 
  | x <= head xs = isSorted xs
  | otherwise   = False

-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels aaTree = leftChildOK aaTree && rightChildOK aaTree && rightGChildOK aaTree

leftChildOK :: AATree a -> Bool
leftChildOK (Node _ (Node _ _ _ lLevel) _ sLevel) = lLevel < sLevel
leftChildOK _ = True

rightChildOK :: AATree a -> Bool
rightChildOK (Node _ _ (Node _ _ _ rLevel) sLevel) 
  = rLevel <= sLevel
rightChildOK _ = True

rightGChildOK :: AATree a -> Bool
rightGChildOK (Node _ _ (Node _ _ (Node _ _ _ rgLevel)rLevel)sLevel)
  | (rLevel == sLevel) && (rgLevel < rLevel)                      = True
  | (rLevel <  sLevel) && (rgLevel < rLevel || rgLevel == rLevel) = True
  |  otherwise = False
rightGChildOK _ = True

isEmpty :: AATree a -> Bool
isEmpty aaTree = case aaTree of
  Empty -> True
  _     -> False

leftSub :: AATree a -> AATree a
leftSub Empty = Empty
leftSub (Node _ left _ _) = left

rightSub :: AATree a -> AATree a
rightSub Empty = Empty
rightSub (Node _ _ right _) = right

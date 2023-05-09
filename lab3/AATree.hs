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
split (Node value left (Node value' _ right'@(Node _ _ _ checklvl) level') level)
  | level == checklvl = Node value' (Node value left right' level) right' (level'+1)
split t = t

skew  :: AATree a -> AATree a
skew (Node value (Node value' left' right' level') right level)
  | level == level' = Node value' left' (Node value right' right level) level'
skew t = t




--inserts given value into AATree, if value already exist in tree -> return input tree unchanged
insert :: Ord a => a -> AATree a -> AATree a
insert value Empty = Node value Empty Empty 1
insert value aaTree@((Node x left right level))
  | value < x  = go (Node x (insert value left) right level)
  | value > x  = go (Node x left (insert value right) level)
  | value == x = aaTree
    where
      go = split . skew
insert _ t = t

inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node x Empty Empty _) = [x]
inorder (Node x left right _) = inorder left ++ [x] ++ inorder right

size :: AATree a -> Int
size Empty = 0
size (Node _ left right _) = size left + size right + 1

height :: AATree a -> Int
height Empty = -1
height aaTree
  | height (leftSub aaTree)  > height (rightSub aaTree) = height (leftSub aaTree)   + 1
  | height (rightSub aaTree) >= height (leftSub aaTree)  = height (rightSub aaTree) + 1



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

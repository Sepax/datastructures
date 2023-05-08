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
split aaTree = aaTree
split (Node value left (Node value' left' right'@(Node _ _ _ checklvl) level') level)
  | level == checklvl = Node value' (Node value left right' level) right' (level'+1)

skew  :: AATree a -> AATree a
skew aaTree = aaTree
skew (Node value left@(Node value' left' right' level') right level)
  | level == level' = Node value' left' (Node value right' right level) level'



--inserts given value into AATree, if value already exist in tree -> return input tree unchanged
insert :: Ord a => a -> AATree a -> AATree a
insert value Empty = Node value Empty Empty 1
insert value aaTree@((Node x left right level))
  | value < x  = go (Node x (insert value left) right level)
  | value > x  = go (Node x left (insert value right) level)
  | value == x = aaTree
    where
      go = split . skew

inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node x Empty Empty _) = [x]
inorder (Node x left right _) = inorder left ++ [x] ++ inorder right

size :: AATree a -> Int
size Empty = 0
size (Node x left right _) = size left + size right + 1

height :: AATree a -> Int
height (Node _ left right _) 
  | height left  > height right = height left   + 1
  | height right >= height left  = height right  + 1

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
isSorted (x:[])    = True
isSorted (x:xs) 
  | x < head xs = isSorted xs
  | x > head xs = False

-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels aaTree@(Node _ _ _ level) = error "checkLevels not implemented"

isEmpty :: AATree a -> Bool
isEmpty aaTree = case aaTree of
  Empty -> True
  _     -> False

leftSub :: AATree a -> AATree a
leftSub (Node x left right level) = left

rightSub :: AATree a -> AATree a
rightSub (Node x left right level) = right

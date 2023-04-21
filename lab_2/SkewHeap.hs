data Skew a = Empty | Node a (Skew a) (Skew a)


emptySkew :: Skew a
emptySkew = Empty

single :: a -> Skew a
single x = Node x Empty Empty


mergeSkew :: Ord a => Skew a -> Skew a -> Skew a
mergeSkew (Node elem (left) (right)) Empty = (Node elem (left) (right))
mergeSkew Empty (Node elem (left) (right)) = (Node elem (left) (right))
mergeSkew Empty Empty                      = Empty
mergeSkew (Node elem1 (l1) (r1)) (Node elem2 (l2) (r2)) 
    | elem1 < elem2 = Node elem1 (merge r1 (Node elem2 (l2) (r2))) (l1)
    | otherwise     = Node elem2 (merge r2 (Node elem1 (l1) (r2))) (l2)


delete ::  Ord a => a -> Skew a -> Skew a
delete _ Empty = Empty
delete x (Node elem (left) (right))
    | x < elem   = Node elem (delete x right) left
    | x > elem   = Node elem left (delete x right)
    | otherwise  = mergeSkew left right


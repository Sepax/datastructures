data Skew a = Empty | Node a (Skew a) (Skew a)


emptySkew :: Skew a
emptySkew = Empty

single :: a -> Skew a
single x = Node x Empty Empty

skewHeap :: Eq a => Skew a -> Skew a -> Skew a
skewHeap x Empty = x
skewHeap Empty y = y
skewHeap
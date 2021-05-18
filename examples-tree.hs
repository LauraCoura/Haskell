-- Binary tree constructor
data Tree a
   = Leaf
   | Node a (Tree a) (Tree a)
   deriving (Eq, Ord, Show)

-- Insert on tree
insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Leaf = Node x Leaf Leaf
insertTree x (Node y l r)
   = case compare x y of
       LT -> Node y (insertTree x l) r
       GT -> Node y l (insertTree x r)
       _  -> Node y l r

-- Turn tree into list
toList :: Tree a -> [a]
toList t = toList' t []
    where
      toList' Leaf ac = ac
      toList' (Node x l r) ac
         = toList' l (x : toList' r ac)

-- Turn list into tree
fromList :: Ord a => [a] -> Tree a
fromList = foldr insertTree Leaf  

-- Implementation of mapTree using foldTree
foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ v Leaf = v
foldTree f v (Node x l r)
     =  f x (foldTree f v l)
            (foldTree f v r)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node x l r)
       = Node (f x) (mapTree f l)
                    (mapTree f r)

mapTree' :: (a -> b) -> Tree a -> Tree b
mapTree' f = foldTree (\x acl acr -> Node (f x) acl acr) Leaf

-- Count number of elements on a tree with foldTree
contTree :: Tree a -> Int
contTree = foldTree (\x acl acr -> 1 + acl + acr) 0

-- Applicative and Functor for tree
instance Functor Tree where  
    fmap f Leaf = Leaf  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

instance Semigroup (Tree a) where
    Leaf <> (Node x leftsub rightsub) = Node x leftsub rightsub
    (Node x leftsub rightsub) <> Leaf = Node x leftsub rightsub
    (Node x leftsub rightsub) <> (Node y leftsub' rightsub') = Node y leftsub' rightsub'

instance Applicative Tree where
    pure x = (Node x Leaf Leaf)
    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Node f leftsub rightsub) <*> (Node x leftsub' rightsub') = Node (f x) 
                                                                ((fmap f leftsub') <> (leftsub <*> leftsub')) 
                                                                ((fmap f rightsub') <> (rightsub <*> rightsub'))
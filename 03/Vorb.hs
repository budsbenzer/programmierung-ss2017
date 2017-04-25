foldl' f a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs

foldl'' f a xs = foldr (\x g -> flip f x . g) id xs a


data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf a) = Leaf (f a)
tmap f (Node a l r) = Node (f a) (tmap f l) (tmap f r)


p :: Tree a -> Tree [a]
p (Leaf a) = Leaf [a]
p (Node a l r) = tmap (a:) $ Node [] (p l) (p r)

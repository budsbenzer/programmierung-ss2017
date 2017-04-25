module Tut03 where
import Prelude hiding (foldl)

data Tree' = Leaf' Int | Branch' Tree' Tree'

countLeaves :: Tree' -> Int
countLeaves (Leaf' _) = 1
countLeaves (Branch' l r) = countLeaves l
                           + countLeaves r

yield :: Tree' -> [Int]
yield (Leaf' x) = [x]
yield (Branch' l r) = yield l ++ yield r

depth :: Tree a -> Int
depth (Leaf x) = 1
depth (Branch x l r) = 1 + depth l `min` depth r
          -- d.h. (min (depth l) (depth r))


path :: Tree a -> Tree [a]
path n = aux n []
  where
    aux :: Tree a -> [a] -> Tree [a]
    aux (Leaf a) l = Leaf (l ++ [a])
    aux (Branch a lb rb) l
      = let l' = l ++ [a]
        in Branch l' (aux lb l') (aux rb l')

-- f xs = foldr (*) 1 (map (^2) (filter even xs))
f = foldr (*) 1 . map (^2) . filter even

data Tree a = Branch a (Tree a) (Tree a) | Leaf a
  deriving Show

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf a) = Leaf (f a)
tmap f (Branch a l r) = Branch (f a)
                          (tmap f l)
                          (tmap f r) 

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a [] = a
foldl f a (x:xs) = foldl f (f a x) xs

foldl' f a xs = foldr f' a (reverse xs)
  where f' x y = f y x

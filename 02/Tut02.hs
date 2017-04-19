module Tut02 where

pack :: [Char] -> [[Char]]
pack [] = []
pack xs = ps : pack (remove (length ps) xs)
  where
    ps = prefix xs

prefix :: [Char] -> [Char]
prefix [] = []
prefix [x] = [x]
prefix (x:y:xs)
  | x == y  = x : prefix (y:xs)
  | otherwise = [x]

remove :: Int -> [Char] -> [Char]
remove 0 xs = xs
remove n [] = []
remove n (x:xs) = remove (n-1) xs

pref' :: [Char] -> ([Char], [Char])
pref' [] = ([], [])
pref' [x] = ([x], [])
pref' (x:y:xs)
  | x == y = (x : ps, rs)
  | otherwise = ([x], y:xs)
  where (ps,rs) = pref' (y:xs)

pack' [] = []
pack' xs = ps : pack' rs
  where (ps,rs) = pref' xs

encode :: [Char] -> [(Int, Char)]
encode xs = e (pack xs)
  where
    e :: [[Char]] -> [(Int, Char)]
    e [] = []
    e (l:ls) = (length l, head l) : e ls

decode :: [(Int, Char)] -> [Char]
decode [] = []
decode ((n,x):xs) = times n x ++ decode xs
  where
    times :: Int -> Char -> [Char]
    times 0 _ = []
    times n c = c : times (n-1) c

rotate [] _ = []
rotate xs 0 = xs
rotate xs n
  | n > 0 = rotate (tail xs ++ [head xs]) (n-1)
  -- | n < 0 = rotate (last xs : init xs) (n+1)
  | otherwise = rotate xs (length xs + n)


max_length xs = maximum (aux xs)
-- evtl xs = [] sonderbehandeln!

aux [] = []
aux (l:ls) = length l : aux ls

module Vorb where

import Prelude hiding (words,unwords)

pack [] = []
pack (x:xs) = ys : pack zs
  where
    (ys, zs) = takeall x (x:xs)
    takeall _ [] = ([], [])
    takeall x (y:ys)
      | x == y = let (us, vs) = takeall x ys 
                 in  (y:us, vs)
      | otherwise = ([], (y:ys))

encode xs = e' (pack xs)
  where e' [] = []
        e' (y:ys) = (length y, head y) : e' ys

decode [] = []
decode ((n, x) : xs) = times n x ++ decode xs
  where
    times 0 x = []
    times n x = x : times (n-1) x

rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate xss@(x:xs) n
  | n == 0 = xss
  | n < 0  = rotate xss (length xss + n)
  | otherwise = rotate (xs ++ [x]) (n - 1)

unwords [] = []
unwords [w] = w
unwords (w:ws) = w ++ " " ++ unwords ws

isSpace c = c `elem` " \t" -- ist c Leerzeichen oder TAB?

words [] = []
words (c:cs)
  | isSpace c = words cs
words cs = let (w,cs') = takeWord cs
           in w : words cs'
  where takeWord [] = ([],[])
        takeWord (c:cs)
          | isSpace c = ([], cs)
          | otherwise = let (w,cs') = takeWord cs
                        in (c:w,cs')

type Queue = ([Int],[Int])
-- es soll für eine Queue (ls,rs) gelten, dass sie die Liste ls ++
-- reverse rs darstellt

-- außerdem soll gelten dass Queue (ls,rs) leer gdw. ls leer ist

-- Wenn man amortized runtime betrachtet, sind die Operationen O(1):
-- im Schnitt wird kaum reverse ausgeführt (vgl Okasaki, Purely
-- Functional Data Structures)

isEmpty ([],[]) = True
isEmpty _ = False

queueify ([],rs) = (reverse rs, [])
queueify q       = q

enqueue i (ls, rs) = queueify (ls, i:rs)

first ([],[]) = error "empty queue"
first (l:_,_) = l

rest ([],[]) = error "empty queue"
rest (_:xs,ys) = queueify (xs,ys)


import Prelude hiding (rem)

sumto :: Int -> Int
sumto 0 = 0
sumto n = n + sumto (n - 1)

add :: Int -> Int -> Int
add n m = n + m

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

sumFacs :: Int -> Int -> Int
sumFacs n m
  -- | n == m     = fac n
  | n > m      = 0
  | otherwise  = fac n + sumFacs (n + 1) m

sumFacs' n m = sum [fac x | x <- [n .. m]]

sumFacs'' n m = sum (map fac [n .. m])

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

prod :: [Int] -> Int 
prod [] = 1
prod (x:xs) = x * prod xs


rev [] = []
rev (x:xs) = rev xs ++ [x]

rev' :: [Int] -> [Int]
rev' xs = aux xs []
  where
    aux :: [Int] -> [Int] -> [Int]
    aux [] ys = ys
    aux (x:xs) ys = aux xs (x:ys)

app :: [Int] -> [Int] -> [Int]
app [] ys = ys
app (x:xs) ys = x : (app xs ys)

rem :: Int -> [Int] -> [Int]
rem n xs = [x | x <- xs, x /= n]

rem' _ []     = []
rem' n (x:xs)
  | n == x    = rem' n xs
  | otherwise = x : rem' n xs

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x] = True
isOrd (x:y:xs) = x <= y && isOrd (y:xs)

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y   = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

fibs :: [Int]
fibs = [fib x | x <- [0..]]

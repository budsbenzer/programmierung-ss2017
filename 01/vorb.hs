fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

sumFacs :: Int -> Int -> Int
sumFacs n m
  | m < n     = 0
  | otherwise = fac n + sumFacs (n + 1) m

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib i = fib (i-1) + fib (i-2)

prod :: [Int] -> Int
prod []     = 1
prod (x:xs) = x * prod xs

append [] ys     = ys
append (x:xs) ys = x : append xs ys

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- oder

rev' :: [Int] -> [Int]
rev' xs = aux xs []
  where
    aux [] ys = ys
    aux (x:xs) ys = aux xs (x:ys)

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x] = True
isOrd (x:y:xs) = x <= y && isOrd (y:xs)

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- auch mit bind-pattern xxs@(x:xs) möglich

fibs :: [Int]
fibs = f' 0
  where
    f' i  = fib i : f' (i+1)

-- Beispiel: take 5 fibs


countBinTrees :: Int -> Int
countBinTrees 0 = 0
countBinTrees 1 = 1
countBinTrees n = go (n-1)
  where
    go 0 = 0
    go m = go (m-1) + countBinTrees (n-1-m) * countBinTrees m


-- Diese Lösung beruht auf den Catalan-Zahlen (s. z.B. Wikipedia).
-- Cₙ = (2n)! / (n+1)!n!
countBinTrees' n
  | even n = 0
  | otherwise = let m = (n-1) `div` 2
                in  fac (2*m) `div` (fac m * fac (m+1)) 

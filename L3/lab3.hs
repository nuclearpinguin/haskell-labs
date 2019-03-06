import Data.List


-- #2
-- fibloop_old :: Integer -> Integer -> Integer -> Integer
-- fibloop_old n n_1 n_2
--   | n <= 1 = n_1 + n_2
--   | otherwise = fibloop_old (n-1) (n_1 + n_2) n_1

-- fib1 :: Integer -> Integer
-- fib1 n = fibloop_old n 1 0


fibloop :: Integer -> Integer -> Integer -> Integer
fibloop n n_1 n_2
  | n <= 1 = n_1 + n_2
  | otherwise = fibloop (n-1) (0 `seq` (n_1 + n_2)) n_1


fib2 :: Integer -> Integer
fib2 n = fibloop n 1 0

main = print (fib2 100000)


-- #3
avg :: [Double] -> Double
avg [] = 0
avg xs = (foldl' (+) 0.0 xs) / fromIntegral( length xs )

-- #4
elem1 :: (Ord a) => [a] -> a -> Bool
elem1 [] _ = False
elem1 (x:xs) y
    | x == y = True
    | otherwise = elem1 xs y


elem2 :: (Ord a) => [a] -> a -> Bool
elem2 [] _ = False
elem2 xs y = foldl' (?) False xs
    where
        a ? b = a || (b==y)

-- #6
decreasing :: (Ord a) => [a] -> Bool
decreasing [] = False
decreasing (x:xs) = fst $ foldl' (?) (True, x) xs
    where
        (s, p) ? b = (s && (p > b) , b)

-- Well, this is not going to work:
-- test_decreasing = decreasing [200, 100 ..]

--  #7
anytrue :: [Bool] -> Bool
anytrue [] = False
anytrue xs = foldr (||) False xs

test_anytrue = anytrue ([False, True, False]++repeat False)

-- #I2
leibnitzpi :: Double
leibnitzpi = 4.0 * (fst $ foldl' (?) (1.0, 1.0) (map (1/) [3, 5 .. 20001]))
    where
        (x, i) ? y = (x  + (-1.0 * i) * y, -1.0 * i)


-- #I3
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort ls ++ pivots ++ quickSort rs
    where
        pivots = filter (==x) (x:xs)
        ls = filter (<x) xs
        rs = filter (>x) xs

        
-- #I7
-- Sum numbers in list form ex. 103 = [1, 0, 3]
sumList :: [Int] -> [Int] -> [Int]
sumList xs ys 
    | lxs > lys = (take d xs)  ++ s
    | otherwise = (take d ys)  ++ s
    where
        lxs = length xs
        lys = length ys
        d = abs (lxs - lys) 
        s = reverse $ zipWith (+) (reverse xs) (reverse ys)
        
-- #I10
-- Lazy filter implementation
lazyFilter :: (a -> Bool) -> [a] -> [a]
lazyFilter c xs = foldr f [] xs
    where
        f a b | (c a) = [a] ++ b | otherwise = b


test = take 5 $ lazyFilter ((== 0) . (`mod` 2)) [1,2..]
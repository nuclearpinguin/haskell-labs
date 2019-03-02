import Data.List

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

{-# LANGUAGE BangPatterns #-}

-- Ord a mean linear order on a
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) = (quickSort lesser) ++ [p] ++ quickSort(greater)
    where
        lesser = filter (<= p ) xs
        greater = filter( >p) xs

myEven :: Integer -> Integer
myEven a = mod a 2


-- Exercise 69.3
-- mean :: [Float] -> Maybe Float
-- mean [] = Nothing
-- mean xs = Just( sum xs / fromInteger( len xs ) )

mySum :: [Float] -> Float
mySum [] = 0.0
mySum (x:xs) = x + mySum(xs)
 
-- thunks are complicated here
-- and we should avoid double recursion
fibSeq :: Int -> Int
fibSeq 0 = 1
fibSeq 1 = 1
fibSeq n = fibSeq(n-1) + fibSeq(n-2)

factor :: Integer -> Integer
factor 0 = 1
factor n = n * factor (n-1)

tylor1 :: Double -> Integer -> Double
tylor1 x n 
        | n > 0 = x ^ (2*n + 1) * (-1)^n / fromInteger(factor (2*n + 1) ) + tylor1 x (n-1)
        | n == 0 = x

-- using the last element of series and present index i 
-- it evaluates next element for a given x 
tylorSinNext :: Double -> Integer -> Double -> Double
tylorSinNext x i last = (last * x^2)  * (-1) /  fromIntegral( (i+1) * (i+2) )

-- helper for tylorSeries
tylorAdderSeries :: Double -> Integer -> Integer -> [Double] -> Double -> [Double]
tylorAdderSeries x i n s last 
    | i <= n+1 = tylorAdderSeries x (i+2) n new_s new_last
    | otherwise = s
    where
        new_last = tylorSinNext x i last
        new_s = s ++ [new_last]

-- creates list of first n elements of Tylor series of sin x
tylorSeries :: Double -> Integer -> [Double]
tylorSeries x n = tylorAdderSeries x 1 n [x] x


tylorSumSeries :: Double -> Integer -> Double
tylorSumSeries x n = sum $ tylorSeries x n 


-- helper for tylorSin
tylorAdder :: Double -> Integer -> Integer -> Double -> Double -> Double
tylorAdder x i n s last 
    | i <= n+1 = tylorAdder x (i+2) n new_s new_last
    | otherwise = s
    where
        new_last = tylorSinNext x i last
        new_s = s + new_last

-- creates list of first n elements of Tylor series of sin x
tylorSin :: Double -> Integer -> Double
tylorSin x n = tylorAdder x 1 n x x

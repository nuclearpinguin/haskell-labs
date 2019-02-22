module Perceptron (fit, predict, score, dot, add) where
    
-- ROSENBLATT PERCEPTRON

-- Given an vectors of features and perceptron weights it predict the class (1, 0)
predict :: [Double] -> [Double] -> Double 
predict (w:ws) xs 
    | val >= 0.0 = 1.0
    | val < 0.0 = 0.0
    where
        val = w + dot xs ws 

-- Executes n epochs of updataing weight on a given batch
fit :: [[Double]] -> [Double] -> Double -> Integer -> [Double]
fit xs ys rate n 
    |(length xs) == (length ys) = fitHelper 1 n rate init_w xs ys
    | otherwise = init_w
    where
        init_w = replicate (length (xs!!0) + 1) 0.0


-- Helper for fitting
fitHelper :: Integer -> Integer -> Double -> [Double] -> [[Double]] -> [Double] -> [Double]
fitHelper i n rate ws xs ys 
    | i <= n = fitHelper (i+1) n rate new_w xs ys
    | otherwise = ws
    where
        new_w = updateWeightsBatch rate ws xs ys

    
-- Updates weights for a single observation (X, y) 
updateWeightsSingle :: Double -> [Double] -> [Double] -> Double -> [Double]
updateWeightsSingle rate (w:ws) xs y = [w + update] ++ add (map (update*) xs) ws
    where
        update = rate * ( y - predict ws xs)


-- Updates weights for a batch of observations ([[X1], [X2], ...], [y1, y2])
updateWeightsBatch :: Double -> [Double] -> [[Double]] -> [Double] -> [Double]
updateWeightsBatch rate (w:ws) (x:xs) (y:ys) 
    | length xs > 0 = updateWeightsBatch rate new_w xs ys
    | otherwise = w:ws
    where
        new_w = updateWeightsSingle rate (w:ws) x y


-- SCORE HELPERS
        
-- TP / ALL
score :: [[Double]] -> [Double] -> [Double] -> Double
score xs ys ws = 1 - fromIntegral ( abs (y - y_hat) )/ fromIntegral (length ys)
    where
        m = map (predict ws) xs
        y = length $ filter (==0.0) ys
        y_hat = length $ filter (==0.0) m


-- LINEAR ALGEBRA HELPERS

-- Dot product of two lists
dot :: [Double] -> [Double] -> Double
dot xs ys 
    | cond = (head xs) * (head ys) + dot (tail xs) (tail ys) 
    | otherwise = 0
    where
        lxs = length xs
        lys = length ys
        cond = (lxs == lys) && lxs >0 && lys >0

-- Element-wise sum of two lists
add :: [Double] -> [Double] -> [Double]
add (x:xs) (y:ys) 
    | cond = [x+y] ++ add xs ys
    | otherwise = [x+y]
    where
        lxs = length xs
        lys = length ys
        cond = (lxs == lys) && lxs >0 && lys >0

        
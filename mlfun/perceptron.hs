-- Rosenblatt Perceptron

-- Dot product of two lists
dot :: [Double] -> [Double] -> Double
dot xs ys 
    | length xs > 0 && length ys > 0 = (head xs) * (head ys) + dot (tail xs) (tail ys) 
    | otherwise = 0


net_input :: [Double] -> [Double] -> Double
net_input xs (w:ws) = w + dot xs ws 
 
predict :: [Double] -> [Double] -> Double 
predict xs ws
    | val >= 0 = 1.0
    | val < 0 = -1.0
    where
        val = net_input xs ws

updateWeightsSingle :: Double -> [Double] -> [Double] -> Double -> [Double]
updateWeightsSingle rate (w:ws) xs y = (w + update) : map (update*) xs
    where
        update = rate * ( y - predict xs ws)


updateWeightsBatch :: Double -> [Double] -> [[Double]] -> [Double] -> [Double]
updateWeightsBatch rate (w:ws) (x:xs) (y:ys) 
    | length xs > 0 = updateWeightsBatch rate new_w xs ys
    | otherwise = w:ws
    where
        new_w = updateWeightsSingle rate (w:ws) x y


fit :: Integer -> Integer -> Double -> [Double] -> [[Double]] -> [Double] -> [Double]
fit i n rate ws xs ys 
    | i <= n = fit (i+1) n rate new_w xs ys
    | otherwise = ws
    where
        new_w = updateWeightsBatch rate ws xs ys

        
test :: [Double]        
test  = fit 1 100 0.001 ws xs ys
    where
        ws = [0.0, 0.0, 0.0]
        xs = [[0.1, 0.5], [0.4, 0.1], [0.15, 0.37], [0.67, 0.09], [0.1, 0.45], [0.17, 0.39]]
        ys = [1.0, -1.0, 1.0, -1.0, 1.0, 1.0]

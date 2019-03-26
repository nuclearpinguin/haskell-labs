import Data.Char

capitalize :: [Char] -> Maybe [Char]
capitalize [] = Nothing
capitalize (x:[]) 
    | isDigit x = Nothing
    | otherwise = Just [toUpper x]
capitalize (x:xs) 
    | isDigit x = Nothing
    | otherwise = case t of
        Nothing -> Nothing
        Just s -> Just ( [toUpper x] ++ s)
        where 
            t = capitalize xs


unaryNumber :: Int -> Maybe [Char]
unaryNumber x
    | x <= 0 = Nothing
    | otherwise = Just $ foldr (++) "" (replicate x "0")


interleave :: [Char] -> [Char] -> Maybe [Char]
interleave [] [] = Just ""
interleave (x:xs) (y:ys) 
    | length xs /= length ys = Nothing
    | otherwise = case t of
        Nothing -> Nothing
        Just s -> Just $ [x, y] ++ s 
        where
            t = interleave xs ys

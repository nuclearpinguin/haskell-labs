factor :: Integer -> Integer
factor 0 = 1
factor n = n * factor (n-1)


myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse(xs) ++ [x]
myReverse [] = []

-- isPalindrome :: Eq a => [a] -> Bool
-- isPalindrome a = a == reverse a

-- listLen :: [a] -> Integer
-- listLen (_:xs) = 1 + listLen(xs)
-- listLen [] = 0

-- makePalindrome :: [a] -> [a]
-- makePalindrome xs = xs ++ reverse(xs)

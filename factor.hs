factor :: Integer -> Integer
factor 0 = 1
factor n = n * factor (n-1)
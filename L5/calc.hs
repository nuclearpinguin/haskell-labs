execute :: [Char] -> Int -> Int -> Int
execute o a b
    | o == "+" = a + b
    | o == "*" = a * b
    | o == "-" = a - b


main :: IO ()
main = do
    putStrLn "Wow! You can do +, -, * on ints! "
    putStrLn "What is the symbol between x and y? "
    o <- getLine
    putStrLn "What is the x? "
    a <- getLine
    putStrLn "What is the y? "
    b <- getLine
    let x = (read a :: Int)
    let y = (read b :: Int)
    print $ execute o x y

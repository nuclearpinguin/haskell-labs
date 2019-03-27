
import Data.List 
import Control.Monad

-- Define board markings
data Mark = E | O | X deriving Eq


instance Show Mark where
    show E = " "
    show O = "O"
    show X = "X"


-- Define Board structure
data Board = Empty
            | Board {
                row1 :: [Mark],
                row2 :: [Mark],
                row3 :: [Mark]
            }


instance Show Board where
    show Empty = unlines $ replicate 3 "  |   |  "
    show (Board r1 r2 r3) = unlines [dsp r1, dsp r2, dsp r3]
        where
            dsp row = intercalate " | " (map show row) 


-- Check if list contains only 1 element 
inRow :: Eq a => a -> [a] -> Bool
inRow _ [] = True
inRow t xs = foldl' (\ a b -> a && (b == t)) True xs


-- Check if a given mark has three in line
inLine :: Mark -> Board -> Bool
inLine _ Empty = False
inLine m (Board r1 r2 r3) = v || h || d
    where
        -- vertical
        v = foldr (||) False $ map (inRow m) [[a, b, c] | (a, b, c) <- zip3 r1 r2 r3]
        -- horizontal
        h = foldr (||) False $ map (inRow m) [r1, r2, r3]
        -- diags
        d = (inRow m [r1 !! 0, r2 !! 1, r3 !! 2]) || (inRow m [r3 !! 0, r2 !! 1, r1 !! 2])


-- change element on i-th position in a given array
change :: a -> Int -> [a] -> [a]
change x _ [] = [x]
change x i xs = l ++ [x] ++ (tail r)
    where
        (l, r) = splitAt i xs


-- Put a mark on a board
addMark :: String -> Mark -> Board -> Board
addMark _ E b = b
addMark p m b@(Board r1 r2 r3)
    | x == 'a' = Board (change m y r1) r2 r3
    | x == 'b' = Board r1 (change m y r2) r3
    | x == 'c' = Board r1 r2 (change m y r3)
    | otherwise = b
    where
        x = head p
        y = -1 + read (tail p) :: Int 
addMark p m b@Empty
    | x == 'a' && r = Board (change m y empty) empty empty
    | x == 'b' && r = Board empty (change m y empty) empty
    | x == 'c' && r = Board empty empty (change m y empty)
    | otherwise = b
    where
        empty = [E, E, E]
        x = head p
        y = -1 + read (tail p) :: Int 
        r = elem y [0, 1, 2]

-- Check if game is over
isFull :: Board -> Bool
isFull Empty = False
isFull (Board r1 r2 r3) = (ch r1) && (ch r2) && (ch r3)
    where
        ch r = foldl' (\a b -> a && (b /= E)) True r

checkGameStatus :: Board -> Mark -> IO()
checkGameStatus b m
    | inline = putStrLn $ "Game over! Player " ++ (show m) ++ " won!"
    | full = putStrLn "Game over! Draw."
    | otherwise = game b nm
    where
        inline = inLine m b
        full = isFull b
        nm = if (m==X)
            then O
            else X
    

-- Loop for the tic-tac-toe game
game :: Board -> Mark -> IO()
game b m = do
    -- Player move
    putStrLn $ "Move of player " ++ show m
    p <- getLine
    let b1 = addMark p m b
    print b1
    checkGameStatus b1 m


main :: IO()
main = do
    putStrLn "<<<< COSMIN TIC-TAC-TOE >>>>"
    putStrLn "To make a move input something like 'a2'."
    -- putStrLn "Who starts? [X / O]"
    putStrLn"Player X starts,"
    -- im <- getLine
    -- let m = read im :: Mark
    print Empty
    game Empty X
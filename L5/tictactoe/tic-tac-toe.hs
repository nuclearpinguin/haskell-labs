import Control.Monad
import Board 
import Helpers

-- Check if game is over or not
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
    putStrLn "Who starts? [X / O]"
    m <- getLine >>= return. (\x -> read x :: Mark)
    -- let m = read im :: Mark
    
    -- Show board coords
    putStrLn "\n To make a move input something like 'a2'."
    putStrLn "  | 1 | 2 | 3"
    putStrLn $ unlines $ map (\(a, b) -> (a ++ b)) (zip ["a | " , "b | ", "c | "] (lines $ show Empty))

    -- Start the game
    game Empty m
    
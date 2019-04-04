module Counter where
import Control.Monad.State.Lazy


-- QuickSort counter

type Counter = State Int Bool

    
lessEqCnt :: Int -> Int -> Counter
lessEqCnt x y = do
    cnt <- get
    case ( x <= y ) of
        True -> put (cnt + 1) >> return True
        False -> put (cnt + 1) >> return False


showCounter :: Counter -> String
showCounter cnt = show $ runState cnt 0


-- Calculates number of comparsions in a quick sort run
quickSortCounter :: [Int] -> Counter
quickSortCounter [] = return False
quickSortCounter (y:ys) = 
    foldl (>>) (return False) [lessEqCnt x y | x <- ys] >>
    foldl (>>) (return False) [lessEqCnt y x | x <- ys] >>
    quickSortCounter (filter (<y) ys) >> quickSortCounter (filter (>=y) ys)

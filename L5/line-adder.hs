module Main (main) where
import System.IO
import Text.Read
import Data.List.Split


adder :: String -> Int
adder s = case m of
    Nothing -> 0
    Just x -> x + 1
    where
        m = readMaybe s


fileHandler :: Handle -> IO [Int]
fileHandler hdl = hGetContents hdl >>= return . \x -> map adder (lines x)


-- Checks extension of file
checkExt :: String -> IO String
checkExt x
    | ext == "txt" = return x
    | otherwise = fail "Wrong extension! Only .txt is allowed :< "
    where
        ext = last $ splitOn "." x


main :: IO()
main = do 
    putStrLn "Provide path to first file"
    path <- getLine >>= \x -> checkExt x

    putStrLn "Name of file to write into"
    name <- getLine >>= \x -> checkExt x


    withFile path ReadMode $ \hdl -> do
        output <- fileHandler hdl
        -- print $ output
        withFile name WriteMode $ \hdl -> do
            hPutStr hdl (unlines $ map show output)

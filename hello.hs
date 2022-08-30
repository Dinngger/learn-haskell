import Data.Char

foreign export ccall hello :: IO ()

hello :: IO ()
hello = print (sum (map (\x -> x * x) ([1..2000000] :: [Int])))

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main = do
    line <- getLine
    if null line
        then return ()
    else do
        putStrLn $ reverseWords line
        main

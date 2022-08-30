import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl folding [] . words
    where folding (x:y:ys) "*" = (x * y):ys
          folding (x:y:ys) "+" = (x + y):ys
          folding (x:y:ys) "-" = (y - x):ys
          folding (x:y:ys) "/" = (y / x):ys  
          folding (x:y:ys) "^" = (y ** x):ys  
          folding (x:xs) "ln" = log x:xs  
          folding xs "sum" = [sum xs] 
          folding xs numberString = read numberString:xs

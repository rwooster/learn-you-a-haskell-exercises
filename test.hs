addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

addLists (x1:y1:xs) (x2:y2:ys) = [x1+x2, y1+y2]



fancyAdd :: (Num a, Show a, Show b, Ord a) => [a] -> [b] -> String
fancyAdd xs@(x1:y1:_) ys@(x2:y2:_)
    | x1 > 10 = "Real big: " ++ show x1
    | y1 > 10 = "Y real big: " ++ show y1
    | otherwise = "xs:" ++ (show xs) ++ " and ys: " ++ (show ys)

dupe xs = f xs []
  where f [] l = l 
        f xs l = f (init xs) l ++ [last xs, last xs] 

max' :: (Ord a) => [a] -> a
max' [a] = a  
max' (a1:as) 
  | a1 > maxSoFar = a1
  | otherwise     = maxSoFar
  where maxSoFar = max' as


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = --- Pivoting at the first element
  let small = quicksort [a | a <- xs, a <= x]
      large = quicksort [a | a <- xs, a > x]
  in small ++ [x] ++ large

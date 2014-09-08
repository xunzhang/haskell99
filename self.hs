
myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "self.myMaximum: maximum of empty list"
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

myReplicate :: Int -> a -> [a]
myReplicate n v
    | n <= 0 = []
    | otherwise = v:myReplicate (n - 1) v

myTake :: (Num i, Ord i) => i -> [a] -> [a] -- Num and Ord is need
myTake n _
    | n <= 0 = []
myTake _ [] = []
myTake n (x:xs) = x:(myTake (n - 1) xs)

myRepeat :: a -> [a]
myRepeat x = x:myRepeat x

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myElem :: (Eq a) => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs)
    | a == x = True
    | otherwise = a `myElem` xs 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallpart = [v | v <- xs, v <= x]
        largepart = [v | v <- xs, v > x]
    in quicksort smallpart ++ [x] ++ quicksort largepart

{-

    4 Problem 4
        (*) Find the number of elements of a list.

        Example in Haskell:

        Prelude> myLength [123, 456, 789]
        3
        Prelude> myLength "Hello, world!"
        13
-}

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' list = acc list 0 -- here acc is an accumulator
    where acc [] n = n
          acc (_:xs) n = acc xs (n + 1)

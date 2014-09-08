{-

    Problem 3
        (*) Find the K'th element of a list. The first element in the list is
        number 1.

        Example:

        * (element-at '(a b c d e) 3)
        c
        Example in Haskell:

        Prelude> elementAt [1,2,3] 2
        2
        Prelude> elementAt "haskell" 5
        'e'
-}

elementAt :: [a] -> Int -> a
elementAt x i = x !! (i - 1)

elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1 = x
elementAt' (_:xs) i = if i > 1 then elementAt' xs (i - 1) else error "Index out of bounds here"
elementAt' _ _ = error "Index out of bounds"

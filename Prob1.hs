{-

    Problem 1
        (*) Find the last element of a list.

        (Note that the Lisp transcription of this problem is incorrect.)

        Example in Haskell:

        Prelude> myLast [1,2,3,4]
        4
        Prelude> myLast ['x','y','z']
        'z'
-}

myLast x = if len == 0 then error "Prob1.myLast: empty list" else x !! (len - 1) where len = (length x)

myLast' x = case len == 0 of True -> error "Prob1.myLast': empty list"
                             False -> x !! (len  - 1) 
            where len = (length x)

myLast'' :: [a] -> a
myLast'' [] = error "Prob1.myLast'': empty list"
myLast'' x = x !! (length x - 1)

myLast''' :: [a] -> a
myLast''' [] = error "Prob1.myLast''': empty list"
myLast''' [x] = x 
myLast''' (_:xs) = myLast''' xs

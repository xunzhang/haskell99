{-

    Problem 2
        (*) Find the last but one element of a list.

        (Note that the Lisp transcription of this problem is incorrect.)

        Example in Haskell:

        Prelude> myButLast [1,2,3,4]
        3
        Prelude> myButLast ['a'..'z']
        'y'
-}

myButLast :: [a] -> a
myButLast [] = error "Prob2.myButLast: not enough elements"
myButLast [x] = error "Prob2.myButLast: also not enough elements"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs 

myButLast' :: [a] -> a
myButLast' [] = error "Prob2.myButLast': not enough elements"
myButLast' [x] = error "Prob2.myButLast': also not enough elements"
myButLast' x = x !! (length x - 2)

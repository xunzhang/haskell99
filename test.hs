foo :: [a] -> [a]
foo [] = error "empty list"
foo (_:xs) = xs

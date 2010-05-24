-- Haskell only allows homogenous lists so I will have to learn hard to
-- find out how to make a datatype that represents a scheme list of symbols
-- subst new old [] = []
-- subst new old (x:xs) =
--         sse x : subst new old xs
--     where
--         sse (x:xs) = subst new old (x:xs)
--         sse x
--           | x == old = new
--           | otherwise = x

-- subst with homogoneous list
subst new old [] = []
subst new old (x:xs)
    | x == old = new : subst new old xs
    | otherwise = x : subst new old xs

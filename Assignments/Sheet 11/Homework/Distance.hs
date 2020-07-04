-- Problem 11.3
{- |
Module: Distance.hs

Find the edit distance between 2 lists using Levenshtein distance string metric
-}
-- a) Code ed
module Distance (ed) where

-- Polymorphic function implements the Levenshtein Distance string metric
-- It takes two lists and returns an Int representing the Levenshtein Distance
-- It pattern matches the base cases:
--      both lists empty    => distance = 0
--      one list empty      => distance = length of the non-empty list
-- It guards cases when:
--      both first elements are equal   => distance = lev (tail firstList) (tail secondList)
--      all other cases => distance = 1 + minimum of [lev (list with appended first element of second list to first list) (secondList),
--                                                    lev (tail firstList) (secondList),
--                                                    lev (firstList with replaced first element with secondList's first element) (secondList)]

ed :: Eq a => [a] -> [a] -> Int
ed [] [] = 0
ed [] ys = length ys
ed xs [] = length xs
ed (x:xs) (y:ys)
    | x == y    = ed xs ys
    | otherwise = 1 + minimum [ed (y:(x:xs)) (y:ys), ed xs (y:ys), ed (y:xs) (y:ys)]
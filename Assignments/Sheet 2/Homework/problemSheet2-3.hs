-- Problem 2.3
-- a) Code isLeapYear
isLeapYear :: Int -> Bool
isLeapYear year = 
	(mod year 4 == 0) && (not (mod year 100 == 0) || (mod year 400 == 0))

-- b) Code isLeapYear'
isLeapYear' :: Integer -> Bool
isLeapYear' year
	| isDivisibleBy 400 = True
	| isDivisibleBy 100 = False
	| isDivisibleBy 4 = True
	| otherwise = False
		where isDivisibleBy num = mod year num == 0

main = do
    print([isLeapYear x | x <- [0,1,4,72,1900,2000]])
    print([isLeapYear' x | x <- [0,1,4,72,1900,2000]])
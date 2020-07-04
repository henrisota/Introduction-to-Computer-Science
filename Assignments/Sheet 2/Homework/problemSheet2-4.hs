-- Problem 2.4
-- a) Code rotate
-- Pattern matching with a recursive function that calls itself with arguments: 
-- an Int that decrements each time going through recursion and a list of Chars 
-- (String)  that takes the first element of the list and puts it at the end of 
-- the list in order to rotate on the left
rotate :: Int -> [a] -> [a]
rotate 0 string = string
rotate n string = rotate (n-1) ((drop 1 string) ++ (take 1 string))

-- b) Code circle
-- List comprehension that applies function rotate each time with argument i 
-- that goes from 0 to length of string - 1
circle :: [a] -> [[a]]
circle string = [((rotate i string)) | i <- [0..(length string - 1)]]

main = do
    print(rotate 0 "abcdef")
    print(rotate 1 "abcdef")
    print(rotate 7 "abcdef")
    print(rotate 7 "")
    print(circle "")
    print(circle "a")
    print(circle "ab")
    print(circle "abc")
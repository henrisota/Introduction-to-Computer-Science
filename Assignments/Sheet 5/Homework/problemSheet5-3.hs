-- Problem 5.3
-- Imported function digitToInt to convert from Char to Int
import Data.Char(digitToInt)

-- Function takes 2 Ints, number and base, and returns a string 
-- Functions works for numbers bigger than 9 that need to be represented by
-- A,B,C,D,E,F for bases bigger than 10
-- It converts 10->A, 11->B, ...
convertOver10 :: Int -> Int -> String
convertOver10 number base
  | remainder == 10 = "A"
  | remainder == 11 = "B"
  | remainder == 12 = "C"
  | remainder == 13 = "D"
  | remainder == 14 = "E"
  | remainder == 15 = "F"
  | otherwise = show remainder
  where remainder = mod number base

-- Function takes a Char and returns an Int
-- It performs the opposite of convertOver10
-- It reverts back A->10, B->11, ...
revertOver10 :: Char -> Int
revertOver10 number
  | number == 'A' = 10
  | number == 'B' = 11
  | number == 'C' = 12
  | number == 'D' = 13
  | number == 'E' = 14
  | number == 'F' = 15
  | otherwise = (digitToInt number :: Int)

-- Function takes 2 Ints, base and counter, and a String, representation of a
-- number in that base, and it returns an Int, the decimal equivalent of the 
-- number by recursively going from the end to the beginning of the String
-- and summing up the values of each digit in String
sumDigits :: Int -> Int -> String -> Int
sumDigits _ _ [] = 0
sumDigits base i lst = ((base ^ i) * (revertOver10 (last lst)) + (sumDigits base (i+1) (init lst)))

-- a) Code toBase
-- Function takes 2 Ints, base and number, and returns a list of Ints
-- which is the representation of number in the base
-- It works by recursively calling itself and appending the remainder of our 
-- number divided by our base at the end of the list
toBase :: Int -> Int -> [Int]
toBase base number
  | base < 2    = []
  | number <= 0 = []
  | otherwise = (toBase base (div number base)) ++ [mod number base]

-- b) Code fromBase
-- Function takes 1 Int, base, 1 list of Ints, representation of a number in 
-- that base, and it returns an Int, the decimal equivalent of the number
-- It works its way from the end of the list and it sums up the values of
-- each digit in list
fromBase :: Int -> [Int] -> Int
fromBase base lst
  | base < 2        = 0
  | null lst        = 0
  | all (== 0) lst  = 0
  | otherwise = sumDigit 0 (reverse lst)
  where 
    sumDigit i [] = 0
    sumDigit i (x:xs)
      | x < 0 = sumDigit  i []
      | otherwise = ((base ^ i) * x) + sumDigit (i + 1) xs

-- c) Code showBase
-- showBin - convenience function to represent a number in binary
-- It appends to the end of the string the remainder of division between 
-- number and 2 and it calls itself recursively with argument the 
-- integer division between number and 2
showBin :: Int -> String
showBin 0 = "0"
showBin 1 = "1"
showBin number
  | mod number 2 == 0 = showBin (div number 2) ++ "0"
  | otherwise = showBin (div number 2) ++ "1"

-- showOct - convenience function to represent a number in octal
-- It appends to the end of the string the remainder of division between 
-- number and 8 and it calls itself recursively with argument the 
-- integer division between number and 8
showOct :: Int -> String
showOct number
  | number < 8 = show number
  | otherwise = (showOct (div number 8)) ++ (show (mod number 8))

-- showHex - convenience function to represent a number in octal
-- It appends to the end of the string the remainder of division between 
-- number and 16 and it calls itself recursively with argument the 
-- integer division between number and 16
-- In case that remainder is between 9 and 16, it appends the respective
-- character in hexadecimal by calling convertOver10
showHex :: Int -> String
showHex 0 = ""
showHex number
  | ((mod number 16) < 16) && ((mod number 16) > 9 ) = (showHex (div number 16) ++ (convertOver10 number 16))
  | number == 0 = ""
  | otherwise = showHex (div number 16) ++ (show (mod number 16))

-- Function takes 2 Ints, base and number, and returns String
-- It converts the decimal number into a number in the base that we got as arg
-- It works by appending to the end of the string, the remainder of division
-- between number and base and it calls itself
-- It calls itself recursively by performing integer division and using the 
-- remainder of the division between number and base to produce the digits
-- that represent the number in that base
-- It utilizes convenience functions showBin, showOct and showHex when
-- base is 2, 8 or 16 respectively
showBase :: Int -> Int -> String
showBase base number
  | number < 0    = ("Error: Number is negative")
  | number == 0   = ""
  | base < 2      = ("Error: Base less than 2")
  | base == 2     = showBin number
  | base == 8     = showOct number
  | base == 16    = showHex number
  | base > 10     = (showBase base (div number base)) ++ (convertOver10 number base)
  | otherwise     = (showBase base (div number base)) ++ (show (mod number base))

-- d) Code readBin
-- readBin - convenience function to calculate decimal representation
-- by calling sumDigits with argument of base as 2
readBin :: String -> Int
readBin lst = sumDigits 2 0 lst

-- readOct - convenience function to calculate decimal representation
-- by calling sumDigits with argument of base as 8
readOct :: String -> Int
readOct lst = sumDigits 8 0 lst

-- readHex - convenience function to calculate decimal representation
-- by calling sumDigits with argument of base as 16
readHex :: String -> Int
readHex lst = sumDigits 16 0 lst

-- Function takes an Int, base, a String, representation of a number in 
-- that base, and returns an Int, the decimal equivalent of that number
-- It calculates the decimal value by calling function sumDigits
-- It utilizes convenience functions readBin, readOct and readHex when
-- base is 2, 8 or 16 respectively
readBase :: Int -> String -> Int
readBase base lst
  | base < 2          = 0
  | null lst          = 0
  | all (== '0') lst  = 0
  | base == 2         = readBin lst
  | base == 8         = readOct lst
  | base == 16        = readHex lst
  | otherwise = (sumDigits base 0 lst)

main = do
  print(map (\b -> toBase b 16) [2..12])
  print(map (\b -> fromBase b [1,1,1]) [2..16])
  print(map (\b -> showBase b 16) [2..16])
  print(map (\b -> readBase b "111") [2..16])
  print(map (\b -> showBase 16 b) [2..16])
  print(map (\b -> readBase 16 b) ["1","F","2F","A3C"])
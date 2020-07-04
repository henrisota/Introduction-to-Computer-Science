-- Problem 3.3
-- a) Code isPrime
-- Pattern matching combined with guards in order to test primality of input
-- Return True if it is prime or return False otherwise
-- We rule out case 1 and 2; Entering a negative input also returns False
-- Next we check if any division of our input with each number from 
-- 2 to sqrt(input) produces a remainder of 0
-- If yes, our input is not prime and we return False
-- Otherwise, our input is a prime number and return True
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
    | (n < 1) = False
    | (length [x | x <- [2 .. ((truncate(sqrt(fromIntegral n))))], mod n x == 0]) > 0 = False
    | otherwise = True

-- b) Code isCircPrime
-- Guards are used to check if our input is a circular prime number
-- First we convert our input x to String, on which we apply function circle to
-- produce all possible rotations of it. Then we map each string from the list
-- of strings we got to function read which converts each string to Integer
-- We map each Integer with isPrime to produce a list of Boolean values
-- Using all function we check if all Booleans are True
-- If all Booleans are True, then x is circular prime, we return True
-- Otherwise x is not a circular prime, we return False
isCircPrime :: Integer -> Bool
isCircPrime x
  | (all (==True) (map isPrime (map (read :: String->Integer) (circle (show x))))) == True = True
  | otherwise = False

-- Code taken from Problem 2.4
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
  print(isPrime (-1))
  print(filter isPrime [2..100])
  print(isCircPrime (-1))
  print(filter isCircPrime [2..100])
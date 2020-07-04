-- Problem 4.3
-- a) Code isSpecialPrime
-- Guards used to check if input that is Integer is a special prime or not
-- Integer is a special prime if it is prime and it is the sum of 2 neighboring
-- primes and 1
-- Function outputs the result as a Boolean value
isSpecialPrime :: Integer -> Bool
isSpecialPrime num 
  -- Comprehend a list made of all the primes smaller than num
  -- Call function checkSum to see if any neighboring primes and 1 equal to num
  | isPrime num == True = checkSum num [x | x <- [1..num-2], isPrime x]
  | otherwise = False

-- Pattern matching combined with guards to recursively iterate through our list
-- by taking 2 elements of our list and checking if their sum + 1 equals to num
-- In case that isn't True check the tail of our list with checkSum
-- If we have arrived at the end of the list (calling checkSum with empty list)
-- our num is not a special prime
checkSum :: Integer -> [Integer] -> Bool
checkSum num [] = False
checkSum num lst
  | ((head (take 2 lst)) + (last (take 2 lst)) + 1) == num = True
  | otherwise = checkSum num (tail lst)

-- Code taken from Problem 3.3
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

main = do
  print(filter isSpecialPrime [2..100])
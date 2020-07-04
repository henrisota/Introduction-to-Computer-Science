-- Problem 6.3
{- |
    Module: Main.hs
-}

import System.IO
import Data.Char
-- import Emoji (Couldn't get this to work)

-- Peek into the content to decide whether we encode or decode.
convert :: String -> String
convert xs
    | null $ filter (\c -> isLetter c && isAscii c) xs = dec xs
    | otherwise = enc xs

-- a) Code enc
-- Function takes a String and returns a String
-- It recursively goes through the String and checks if the head of the String
-- is an upper or lower case character
-- If so, it converts them to emojis
-- Else it leaves the character as it is and calls itself with the String tail
enc :: String -> String
enc [] = ""
enc (x:xs)
    | isLower x = [(toEnum) ((fromEnum x) + 128415)] ++ enc xs
    | isUpper x = [(toEnum) ((fromEnum x) + 127935)] ++ enc xs
    | otherwise = [x] ++ enc xs

-- b) Code dec
-- Function takes a String and returns a String
-- It recursively goes through the String and checks if the decimal value of
-- is between a certain interval of 26 numbers after 128512 or 128000
-- If so, it converts them back to their respective alphabetic character
-- Else it leaves the character as it is and calls itself with the String tail
dec :: String -> String
dec [] = ""
dec (x:xs)
    | ((fromEnum x) >= 128512 && (fromEnum x) <= 128537) = [(toEnum) ((fromEnum x) - 128415)] ++ dec xs
    | ((fromEnum x) >= 128000 && (fromEnum x) <= 128025) = [(toEnum) ((fromEnum x) - 127935)] ++ dec xs
    | otherwise = [x] ++ dec xs
    
main = do
    contents <- getContents
    putStr $ convert contents
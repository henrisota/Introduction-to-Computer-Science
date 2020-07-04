-- Problem 11.3
{- |
Module: Distance.hs

Unit tests for the Distance module.
-}

-- b) Code Unit tests for Distance Module
module Main where

    import Distance
    import Test.HUnit
    
    tests = TestList [
                TestCase (assertEqual "" 0 $ ed "" ""),
                TestCase (assertEqual "" 8 $ ed "" "checkbox"),
                TestCase (assertEqual "" 10 $ ed "lumberjack" ""),
                TestCase (assertEqual "" 1 $ ed "throw" "throws"),
                TestCase (assertEqual "" 3 $ ed "throw" "throwing"),
                TestCase (assertEqual "" 9 $ ed "kickboxing" "oxygenize"),
                TestCase (assertEqual "" 9 $ ed "oxygenize" "kickboxing"),
                TestCase (assertEqual "" 10 $ ed "kickboxing" "oxygenizes"),
                TestCase (assertEqual "" 3 $ ed [1.0,3.4,5.2,4.3] [ 1.0]),
                TestCase (assertEqual "" 2 $ ed [1,2,3] [1,2,3,4,5]),
                TestCase (assertEqual "" 4 $ ed [1,2,8,9] [1,2,8,9,3,9,2,3]),
                TestCase (assertEqual "" 3 $ ed ['k','i','t','t','e','n'] ['s','i','t','t','i','n','g'])
            ]
    
    main = runTestTT tests
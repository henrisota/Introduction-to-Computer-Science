-- Problem 10.3
{- |
Module: Shape.hs
-}
module Shape where

    -- Function triangleArea' takes 3 points and returns the area (a triangle) bounded by the lines connecting them
    -- It calculates the area by finding the lengths of the segments between the points and Heron's formula
    -- Although exact theoretically, it gives a slightly inexact answer
    --triangleArea' :: Point -> Point -> Point -> Double
    --triangleArea' p1 p2 p3 = sqrt(s * (s - firstSide) * (s - secondSide) * (s - thirdSide))
    --    where
    --        firstSide = sqrt(((x p2) - (x p1))^2 + ((y p2) - (y p1))^2)
    --        secondSide = sqrt(((x p3) - (x p2))^2 + ((y p3) - (y p2))^2)
    --        thirdSide = sqrt(((x p3) - (x p1))^2 + ((y p3) - (y p1))^2)
    --        s = (firstSide + secondSide + thirdSide) / 2
    
    -- Function triangleArea takes 3 points and returns the area (a triangle) bounded by the lines connecting them
    -- It calculates the area using the determinant method
    triangleArea :: Point -> Point -> Point -> Double
    triangleArea p1 p2 p3 = ((x p1) * ((y p2) - (y p3)) + (x p2) * ((y p3) - (y p1)) + (x p3) * ((y p1) - (y p2))) / 2
    
    -- Functions minx, maxx, miny, maxy take 3 points as input and return a double
    -- Respectively they find minimum x, maximum x, minimum y, maximum y between the 3 points
    minx :: Point -> Point -> Point -> Double
    minx a b c = (x a) `min` (x b) `min` (x c)
    
    maxx :: Point -> Point -> Point -> Double
    maxx a b c = (x a) `max` (x b) `max` (x c)
    
    miny :: Point -> Point -> Point -> Double
    miny a b c = (y a) `min` (y b) `min` (y c)
    
    maxy :: Point -> Point -> Point -> Double
    maxy a b c = (y a) `max` (y b) `max` (y c)
    
    data Point = Point { x :: Double, y :: Double } deriving (Show)
        
    -- Rectangles
        
    data Rectangle = Rectangle { p1 :: Point, p2 :: Point } deriving (Show)
    
    -- Circles
    
    data Circle = Circle { m :: Point, r :: Double } deriving (Show)
    
    -- Triangles
    
    data Triangle = Triangle { a :: Point, b :: Point, c :: Point } deriving (Show)
    
    -- a) Code Area
    class Area a where
        area :: a -> Double
        
    instance Area Rectangle where
        area (Rectangle p1 p2) = ((x p2) - (x p1)) * ((y p2) - (y p1))
    
    instance Area Circle where
        area (Circle m r) = pi * r * r
    
    instance Area Triangle where
        area (Triangle a b c) = triangleArea a b c
    
    -- b) Code BoundingBox
    class (Area a) => BoundingBox a where
        bbox :: a -> Rectangle
    
    instance BoundingBox Rectangle where
        bbox (Rectangle p1 p2) = Rectangle { p1 = p1 , p2 = p2 }
    
    instance BoundingBox Circle where
        bbox (Circle m r) = Rectangle p1 p2
            where
                p1 = Point ((x m) - r) ((y m) - r)
                p2 = Point ((x m) + r) ((y m) + r)
    
    instance BoundingBox Triangle where
        bbox (Triangle a b c) = Rectangle p1 p2
            where
                p1 = Point (minx a b c) (miny a b c)
                p2 = Point (maxx a b c) (maxy a b c)
    
    pa = Point { x = 0, y = 0 }
    pb = Point { x = 10, y = 10 }
    pc = Point { x = 0, y = 20 }
    box = Rectangle { p1 = pa, p2 = pb }
    circle = Circle { m = pa, r = 10 }
    triangle = Triangle { a = pa, b = pb, c = pc }
    
    main = do
        print(area box)
        print(area circle)
        print(area triangle)
        print(bbox box)
        print(bbox circle)
        print(bbox triangle)
        print(area $ bbox box)
        print(area $ bbox circle)
        print(area $ bbox triangle)
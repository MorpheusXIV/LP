    --1.Implement a function eql :: [Int] -> [Int] -> Bool that tells wether two lists of integers are equal.
    
eql :: [Int] -> [Int] -> Bool
eql lx ly = (length lx == length ly) && (and $ zipWith (==) lx ly)
    
    --2.Implement a function prod :: [Int] -> Int that returns the product of a list of integers.
    
prod :: [Int] -> Int
prod lx = foldl (*) 1 lx
    
    --3.Implement a function prodOfEvens :: [Int] -> Int that returns the product of all even numbers of a ---list of integers.
    
prodOfEvens :: [Int] -> Int
prodOfEvens x = prod $ filter even x 
    
    --4.Implement a function powersOf2 :: [Int] that generates the list of all the powers of 2.
    
powersOf2 :: [Int]
powersOf2 = iterate (*2) 1
    
    --5.Implement a function scalarProduct :: [Float] -> [Float] -> Float that returns the dot product of two lists of float numbers with the same size.
    
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct lx ly = foldl (+) 0 $ zipWith (*) lx ly 
    
    

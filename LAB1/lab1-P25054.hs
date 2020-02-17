--1.Define a function myLength :: [Int] -> Int that, given a list of integers, returns its length.

myLength :: [Int] -> Int

myLength [] = 0
myLength (x:lx) = 1 + myLength lx

--2.Define a function myMaximum :: [Int] -> Int that, given a non-empty list of integers, returns its maximal element.

myMaximum :: [Int] -> Int

myMaximum [x] = x
myMaximum (x:lx)
    | x > mx = x
    | otherwise = mx
        
    where mx = myMaximum lx
 
--3.Define a function average :: [Int] -> Float that, given a non-empty list of integers, returns its average.

totalS :: [Int] -> Float

totalS [] = 0.0
totalS (x:lx) = fromIntegral x + (totalS lx)
        
average :: [Int] -> Float
average lx = totalS(lx)/fromIntegral(myLength(lx))

--4.Define a function buildPalindrome :: [Int] -> [Int] that, given a list, returns its palindrome that starts with the reserved list.

buildPalindrome :: [Int] -> [Int]

buildPalindrome lx = reverse lx ++ lx

--5.Define a function remove :: [Int] -> [Int] -> [Int] that given a list of integers x and a list of integers y, returns x after having removed all the ocurrences of the elements in y.

remove :: [Int] -> [Int] -> [Int]
remove lx [] = lx
remove [] _ = []
remove (x:lx) ly
    | elem x ly = remove lx ly
    | otherwise = (x:remove lx ly)

--6.Define a function flatten :: [[Int]] -> [Int] that flattens a list of lists yielding a single list of elements.

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (lx:[]) = lx
flatten (x:lx) = x ++ flatten lx

--7.Define a function oddsNevens :: [Int] -> ([Int],[Int]) that, given a list of integers, returns two lists: Onw with all the even numbers and one with all the odd numbers, each of them in the same relative order as in the original list.

getOdds :: [Int] -> [Int]
getOdds [] = []
getOdds (x:lx)
    | mod x 2 == 1 = (x:getOdds lx)
    | otherwise = getOdds lx
    
getEvens :: [Int] -> [Int]
getEvens [] = []
getEvens (x:lx)
    | mod x 2 == 0 = (x:getEvens lx)
    | otherwise = getEvens lx


oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens lx = (getOdds lx , getEvens lx)

--8.Define a function primeDivisors :: Int -> [Int] that returns the list of prime divisors of a non-zero natural.

isPrime :: Int -> Bool
isPrime n 
    | n >= 2 = isPrime' n 2
    | otherwise = False
    where 
        isPrime' n d
            | d > div n 2 = True
            | mod n d == 0 = False
            | otherwise = isPrime' n (d+1)
            
getDivisors :: Int -> Int -> [Int]
getDivisors x y
    | isPrime y && mod x y == 0 = (y:getDivisors d y)
    | x >= 2 = getDivisors x (y+1)
    | otherwise = []
    where
        d = div x y
            
primeDivisors :: Int -> [Int]
primeDivisors x 
    | x >= 2 = getDivisors x 2
    | otherwise = []

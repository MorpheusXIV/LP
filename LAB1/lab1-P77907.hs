absValue :: Int -> Int
absValue n
	| n >= 0 = n
	| otherwise = -n


power :: Int -> Int -> Int 
power x n 
	| n == 0 = 1
	| otherwise = x * power x (n - 1)

isPrime :: Int -> Bool
isPrime n 
    | n >= 2 = isPrime' n 2
    | otherwise = False
	where 
		isPrime' n d
			| d > div n 2 = True
			| mod n d == 0 = False
			| otherwise = isPrime' n (d+1)

slowFib :: Int -> Int 

slowFib n
	| n == 0 = 0
	| n == 1 = 1
	| otherwise = slowFib(n-1) + slowFib(n-2)

quickFib :: Int -> Int

quickFib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = quickFib' n 0 1
        where
            quickFib' n a s
                | n == 0 = a
                | n == 1 = s
                |otherwise = quickFib' (n-1) s (a+s) 

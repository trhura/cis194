fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

fibs1 :: [Integer]
fibs1 = map fib1 [0..]

fib2 :: [Integer] -> [Integer]
fib2 [] = [0]
fib2 (_:[]) = [0, 1]
fib2 (x:y:[]) = [x, y, x + y]
fib2 (x:xs) = x: fib2 xs

fibs2 :: [Integer]
fibs2 = map last (iterate fib2 [0, 1])
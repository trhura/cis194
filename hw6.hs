import Data.List

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

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a next) = a:streamToList next

instance Show a => Show (Stream a) where
    show = unwords . map show . (take 30) . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Stream a next) = Stream (fn a) (streamMap fn next)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn a = Stream a (streamFromSeed fn (fn a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

positives :: Stream Integer
positives = streamFromSeed (+1) 1

evens :: Stream Integer
evens = streamFromSeed (+2) 2

powersOfTwo :: Integer -> [Integer]
powersOfTwo i = map (2^) [0..(floor (logBase 2 n))] where n = fromInteger i

findLargestDivisorPow2 :: Integer -> Integer
findLargestDivisorPow2 n
    | n < 2 = 0
    | otherwise = fromIntegral $ last $ findIndices (== 0) $ map (rem n) (powersOfTwo n)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a next) b = Stream a (interleaveStreams b next)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap findLargestDivisorPow2 evens)
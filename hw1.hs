import Data.Char

-- -- toDigits convert an number to array of its digits
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- toDigitsRev reverse return the digits in reverse as array
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `quot` 10)

-- doubleNext double the every other item in list
doubleNext :: Bool -> [Integer] -> [Integer]
doubleNext _ [] = []
doubleNext True (x:xs)  = x + x : doubleNext False xs
doubleNext False (x:xs) = x : doubleNext True xs

-- doubleEveryOther double every other number beginning from the right,
--  that is, the second-to-last, fourth-to-last, etc ..
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse. (doubleNext False) . reverse

-- sumDigits sum all the digits in Integer list
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum . toDigits) x + sumDigits xs

-- validate check whether the given credit card is valid or not
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0


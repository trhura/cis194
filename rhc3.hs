import Data.List hiding (intersperse)

-- len imitates the length function
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

-- mean computes the mean of any list
mean :: [Float] -> Float
mean xs = sum xs / (fromIntegral $ len xs)

-- convert a list to Palindrome
convertToPalindrome :: [a] -> [a]
convertToPalindrome [] = []
convertToPalindrome xs = xs ++ reverse xs

-- check whether a list is palindrome or not
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (==) xs $ reverse xs

-- sort lists by length
sortBySubListLength :: [[a]] -> [[a]]
sortBySubListLength = sortOn len

-- joins a list of lists together using a separator value.
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse s (x:[]) = s : x
intersperse s (x:xs) = x ++ (intersperse s xs)

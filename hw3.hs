module Golf where

import Data.List

getnth :: Int -> [a]  -> [a]
getnth nth lst = [elm | (idx, elm) <- zip [1..] lst, idx `mod` nth == 0]

skips :: [a] -> [[a]]
skips lst = [ getnth i lst | i <- [1..(length lst)]]

chunks :: Int -> [a] -> [[a]]
chunks n lst
    | length lst < n = []
    | otherwise = [take n lst] ++ chunks n (drop 1 lst)

localMaxima :: Ord a => [a] -> [a]
localMaxima lst = [b | (a:b:c:[]) <- (chunks 3 lst), b > a && b > c]

count :: Eq a => a -> [a] -> Int
count elm lst = length $ filter (== elm) lst

counts :: (Num a, Enum a, Eq a) => [a] -> [Int]
counts lst = [count x lst | x <- [0..9]]

histLine :: Ord a => [a] -> a -> [Char]
histLine counts lineNum = map (\x -> if x >= lineNum then '*' else ' ') counts

histLines :: (Num a, Enum a, Eq a) => [a] -> [[Char]]
histLines lst = reverse $ map (histLine cnts) [1..(maximum cnts)]
                    where cnts = counts lst

histogram lst = intercalate "\n" $
                    histLines lst ++ [take 10 (repeat '=')] ++ [['0'..'9']] ++ ["\n"]





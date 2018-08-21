module Golf where

getnth :: Int -> [a]  -> [a]
getnth nth lst = [elm | (idx, elm) <- zip [1..] lst, idx `mod` nth == 0]

skips :: [a] -> [[a]]
skips lst = [ getnth i lst | i <- [1..(length lst)]]

chunks :: Int -> [a] -> [[a]]
chunks n lst
    | length lst < n = []
    | otherwise = [take n lst] ++ chunks n (drop 1 lst)

localMaxima :: (Ord a) => [a] -> [a]
localMaxima lst = [b | (a:b:c:[]) <- (chunks 3 lst), b > a && b > c]
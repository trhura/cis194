
fun1 :: [Integer] -> Integer
fun1 = foldl (*) 1 . map (subtract 2) . filter even

-- 2. fun2 :: Integer -> Integer
fun2  = foldl (+) 0 . takeWhile (/= 0) . iterate (\y -> if (even y) then (y `div` 2) else (3 * y + 1))

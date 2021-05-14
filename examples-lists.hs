-- Smaller int on list
smaller :: Int -> Int -> Int
smaller x y
    | x == y = x
    | x > y  = y
    | x < y  = x

minList :: [Int] -> Int
minList [] = 0
minList [x] = x
minList (x:xs) = smaller x (minList xs)

-- Position of x on list xs
indexOf :: Int -> [Int] -> Int
indexOf x ys = indexOf' x ys 0
    where
        indexOf' x [] i = -1
        indexOf' x (z:zs) i
            | x == z = i
            | otherwise = indexOf' x zs (i+1)

-- Take first n elements of list xs
takeList :: Int -> [Int] -> [Int]
takeList _ [] = []
takeList n (x:xs)
    | n > 0 = x : takeList (n-1) xs
    | otherwise = takeList n []

-- Remove first n elements of list xs
dropList :: Int -> [Int] -> [Int]
dropList _ [] = []
dropList n (x:xs)
    | n /= 0 = dropList (n-1) xs
    | otherwise = x : xs

-- Remove all occurrences of x on list xs
removeAll :: Int -> [Int] -> [Int]
removeAll x xs = ys
    where
        ys = [y | y <- xs, y /= x]

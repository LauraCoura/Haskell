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

-- Return all elements on a list that are in a certain interval
-- Versions: with filter and with recursion. 
interval :: Int -> Int -> Int -> Bool
interval x y z
    | z >= x && z <= y = True
    | otherwise = False

intFilter :: Int -> Int -> [Int] -> [Int]
intFilter x y zs = filter (\z -> z >= x && z <= y) zs

intRec :: Int -> Int -> [Int] -> [Int]
intRec _ _ [] = []
intRec x y (z:zs)
    | interval x y z = z : intRec x y zs
    | otherwise = intRec x y zs

-- Return the sum of squares of the odds elements on a list
-- Versions: with list comprehension and with filter. 
sumOddQ :: [Int] -> Int
sumOddQ xs = sum [x^2 | x <- xs, odd x]

sumOddQ' :: [Int] -> Int
sumOddQ' = sum . map (^2) . filter (odd)

-- Concatenation of lists using foldr
concatList :: [a] -> [a] -> [a]
concatList xs ys = foldr (:) ys xs

-- Function takeWhile with foldr
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x acc -> if f x then x : acc else []) []

-- Function all with foldr and recursion
allRec :: (a -> Bool) -> [a] -> Bool
allRec _ [] = True
allRec p (x:xs)
    | p x = allRec p xs
    | otherwise = False

allFoldr :: (a -> Bool) -> [a] -> Bool
allFoldr p = foldr step True
    where
        step x ac = p x && ac

-- Function concatMap with composition and with foldr
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat . map f

concatMapF :: (a -> [b]) -> [a] -> [b]
concatMapF f = foldr step []
    where
        step x ac = f x ++ ac

-- Applicative and Functor for list
data List a
  = Nil
  | Cons a (List a)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a x) = (Cons (f a) (fmap f x))

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Applicative List where
    pure x = (Cons x Nil)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)
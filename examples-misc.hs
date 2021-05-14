-- All the perfect numbers in a certain interval 
isPerfect :: Int -> Bool
isPerfect x =
    if sum (init (factors x)) == x
        then True
    else False

perfects :: Int -> [Int]
perfects 0 = []
perfects n = xs
    where
        xs = [x | x <- [1..n], isPerfect x] 

-- All the prime numbers in a certain interval 
isPrime :: Int -> Bool
isPrime x =
    if sum (factors x) == x + 1
        then True
    else False

primes :: Int -> [Int]
primes 0 = []
primes 1 = []
primes n = xs
    where
        xs = [x | x <- [2..n], isPrime x]

module Ch0 where

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

--compose a list containing all of n's divisors greaters than 1
--if the list is empty, the number is prime
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null [x | x <- [2..isqrt n], n `mod` x == 0]

primeFilter :: [Integer] -> [Integer]
primeFilter = filter isPrime

primesOfForm :: Integer -> Integer -> Integer -> [Integer]
primesOfForm x y n = [go a | a <- [1..n], go a < n, isPrime $ go a]
    where go a = x*a + y

q7 = head [go x | x <- [1,2..], go x > 31, isPrime $ go x]
    where go x = 2^x - 1
q8 = head [go x | x <- [1,2..], go x > 17, isPrime $ go x]
    where go x = 2^x + 1

isPerfect :: Integer -> Bool
isPerfect n = total == n
    where total = sum [x | x <- [1..n `div` 2], n `mod` x == 0]

perfects = [x | x <- [6,8..], isPerfect x]

fact n = if n < 1 then 1 else n * fact (n - 1)

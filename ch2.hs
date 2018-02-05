
module Ch2 where

import Ch0
import Data.List
--n>1 and n is an integer, then n can be written as the product of primes

numDivisors  :: Integer -> Int
numDivisors x = (length $ divisors x) + 2

f20 :: Int -> Int -> Int
f20 x y = 2^(x) + 3^(y)

f22 = [x | x <- [1..], numDivisors x == 6]

divisors n = [x | x <- [2..(n `div` 2)], n `mod` x == 0] 

concatInteger xs = read $ concat $ map show xs :: Integer

x240 = map concatInteger $ permutations [1..5]
x240' = [x | x <- x240, x `mod` 11 == 0]

--return the prime factorization of x
primeFactors :: Integer -> [Integer]
primeFactors x 
    | isPrime x = [x]
    | otherwise = primeFactors (fst factor) ++ primeFactors (snd factor)
        where factor = nextFactors x 2

--next factors of x
--check if y divides x, if yes return y*z where z = x/y
--otherwise increment y and check divisibility again
nextFactors :: Integral t => t -> t -> (t, t)
nextFactors x y
    | y > x        = (0, 0)
    | snd res == 0 = (y, fst res)
    | otherwise    = nextFactors x (y + 1)
        where res = divMod x y

primeFactors' :: Integer -> [Char]
primeFactors' x = formatFactorOutput factors (nub factors)
                          where factors = primeFactors x

formatFactorOutput [] _      = []
formatFactorOutput _ []      = []
formatFactorOutput xs (y:ys) = "(" ++ show y ++ "^"
                                ++ (show $ length $ filter (==y) xs) 
                                ++ ")" ++ formatFactorOutput xs ys
--all primes from 2 to x
primeSieve :: Integer -> [Integer]
primeSieve x = if x < 2 then [] else sieveFilter [2..x] 

sieveFilter [] = []
sieveFilter (x:xs) = x : sieveFilter (filter (\n -> n `mod` x /= 0) xs)


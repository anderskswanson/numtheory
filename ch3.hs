
module Ch3 where

import Ch2

sigma n = sum $ divisors n

--by deriving s1(n) as the sum of the reciprocals of divisors of n
s1 n = sum [1 / (fromIntegral x) | x <- divisors n]

--by defining s1(n) in terms of sigma(n)
s1' n = (fromIntegral $ sigma n) / (fromIntegral n)

--sum of the number of divisors of n's divisors
fPrimes n = sum $ map numDivisors $ divisors n

seriesSum n = n*(n+1) / 2

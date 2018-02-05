
module Ch1 where

myGcd :: Integer -> Integer -> Integer
myGcd x y = if a > b then go a b else go b a
    where 
        a = abs x
        b = abs y
        go c 0 = c
        go 0 d = d
        go c d = go d $ c `mod` d
        
myLcm :: Integer -> Integer -> Integer
myLcm 0 _ = error "zero for args"
myLcm _ 0 = error "zero for args"
myLcm x y = abs $ (x * y) `div` (myGcd x y)

gcds :: [Integer] -> [Integer] -> [Integer]
gcds x y = [myGcd a b | a <- x, b <- y]

lcms :: [Integer] -> [Integer] -> [Integer]
lcms x y = [myLcm a b | a <- x, b <- y]

gcdTimesLcm :: [Integer] -> [Integer] -> [Integer]
gcdTimesLcm x y = zipWith (*) (lcms x y) (gcds x y)

divides :: Integer -> Integer -> Bool
divides x y = y `mod` x == 0

divMod' x y = (x `div` y, x `mod` y)

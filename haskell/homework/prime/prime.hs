--(1)
devide :: Integer -> [Integer] -> Bool
devide n [] = False
devide n (x : xs) = n `mod` x == 0 || devide n xs 

prime1 :: Integer -> Bool
prime1 1 = False
prime1 2 = True
prime1 n = if devide n [2..ceiling (sqrt (fromIntegral n))] then False else True



prime3 :: Integer -> Integer -> Bool
prime3 x y = if y ^ 2 <= x 
                then (if x `mod` y == 0 || x `mod` (y + 2) == 0
                            then False
                            else prime3 x (y+6))
                else True

prime2 :: Integer -> Bool
prime2 x = if x `mod` 2 == 0 || x `mod` 3 == 0
                then False
                else prime3 x 5

prime :: Integer -> Bool
prime 1 = False
prime x = if x <= 3 
                then (if x > 1 
                            then True
                            else prime2 x)
                else prime2 x

primesFrom :: Integer -> [Integer]
primesFrom x = if prime x 
                    then x : primesFrom(x+1)
                    else primesFrom(x+1)

allPrimes :: [Integer]
allPrimes = primesFrom 2


primesUntilN :: Integer -> [Integer]
primesUntilN 0 = []
primesUntilN x = if prime x 
                    then x : primesUntilN (x - 1)
                    else primesUntilN (x - 1)

twinPrimes_h :: [Integer] -> Integer
twinPrimes_h [x, y] = if abs(x-y) == 2
                            then 1
                            else 0

twinPrimes_h (x:y:xs)= if abs(x-y) == 2
                        then 1 + twinPrimes_h (y:xs) 
                        else twinPrimes_h (y:xs)
                        

twinPrimes :: Integer -> Integer
twinPrimes x = twinPrimes_h (primesUntilN x) 
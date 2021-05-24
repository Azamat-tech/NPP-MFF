googolMod  :: Integer -> Integer -> Integer
googolMod 0 k = 1
googolMod n k = (10^10) `mod` k * googolMod (n-1) k 
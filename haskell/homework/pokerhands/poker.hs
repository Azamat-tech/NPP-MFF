import Data.List

-- counts the number of elements of an Int in the list [Int]
numberOfElements :: Int -> [Int] -> Int
numberOfElements _ [] = 0
numberOfElements x (y:ys)
    | x == y = 1 + numberOfElements x ys
    | otherwise = numberOfElements x ys

--removes all the values Int from the [Int]
removeValueFromList :: Int -> [Int] -> [Int]
removeValueFromList _ [] = []
removeValueFromList x (y:ys)
    | x == y = removeValueFromList x ys
    | otherwise = y : removeValueFromList x ys

countValue :: [Int] -> [(Int, Int)]
countValue [] = []
countValue (x:xs) = (numberOfElements x (x:xs), x) : countValue (removeValueFromList x (x:xs))

-- [3, 8, 3, 8, 4] to the list [(2, 8), (2, 3), (1, 4)].
countValueMap :: [Int] -> [(Int, Int)]
countValueMap xs = reverse (sort (countValue xs))

isFourOfAKind :: [(Int,Int)] -> Bool
isFourOfAKind xs = if fst (head xs) == 4
                    then True
                    else False

fourOfAKind :: [(Int, Int)] -> [(Int, Int)] -> Bool
fourOfAKind ((x1count, x1value):xs) ((y1count, y1value):ys) = 
    if x1count > y1count 
        then True
        else if x1count == y1count
            then (if x1value < y1value
                then False
                else True)
            else False

isFullHouse :: [(Int, Int)] -> Bool
isFullHouse (x:y:xs) = if fst x == 3 && fst y == 2 
                            then True
                            else False
  
fullHouse :: [(Int, Int)] -> [(Int, Int)] -> Bool
fullHouse ((x1count, x1value):(x2count, x2value):xs) ((y1count, y1value):(y2count, y2value):ys) = 
    if x1count > y1count || x2count > y2count
        then True
        else if x1count == y1count && x2count == y2count
            then (if x1value < y1value
                    then False
                    else True)
            else False

isThreeOfAKind :: [(Int, Int)] -> Bool
isThreeOfAKind (x:xs) = if length (x:xs) == 3 && fst x == 3
                            then True
                            else False

threeOfAKind :: [(Int, Int)] -> [(Int, Int)] -> Bool
threeOfAKind ((x1c,x1v):(x2c,x2v):xs) ((y1c,y1v):(y2c,y2v):ys) = 
    if x1c > y1c
        then True
        else if x1c == y1c
            then (if x1v < y1v
                    then False
                    else True)
            else False

isTwoPair :: [(Int, Int)] -> Bool
isTwoPair (x:xs) = if length (x:xs) == 3 && fst x == 2
                            then True
                            else False

twoPair :: [(Int, Int)] -> [(Int, Int)] -> Bool
twoPair ((x1c,x1v):(x2c,x2v):xs) ((y1c,y1v):(y2c,y2v):ys) = 
    if x1c > y1c || x2c > y2c
        then True
        else if x1c == y1c && x2c == y2c
            then (if x1v > y1v || x2v > y2v
                    then True
                    else False)
            else False



better :: [Int] -> [Int] -> Bool
better xs ys = 
    let xl = countValueMap xs
        yl = countValueMap ys
    in if isFourOfAKind xl || isFourOfAKind yl 
            then fourOfAKind xl yl
            else if isFullHouse xl || isFullHouse yl
                    then fullHouse xl yl
                    else if isThreeOfAKind xl || isThreeOfAKind yl
                            then threeOfAKind xl yl
                            else if isTwoPair xl || isTwoPair yl
                                    then twoPair xl yl
                                    else if xl < yl
                                            then False
                                            else True
        
            
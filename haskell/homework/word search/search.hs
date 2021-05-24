import Data.List
import Data.Char

upper :: [Char] -> [Char]
upper [] = []
upper (x:xs) = toUpper x : upper xs

horizontal :: String -> [String] -> Int
horizontal _ [] = 0
horizontal t (x:xs) = sum [ 1 | r <- tails (upper x), t `isPrefixOf` r ] + horizontal t xs

checkMatrix :: String -> [String] -> [String] -> [Int]
checkMatrix _ _ [] = []
checkMatrix string xs (y:ys)
    | string == "horizontal" = horizontal (upper y) xs : checkMatrix "horizontal" xs ys 
    | string == "horizontalR" = horizontal (upper (reverse y)) xs : checkMatrix "horizontalR" xs ys

checkTransposeMatrix :: String -> [String] -> [String] -> [Int] 
checkTransposeMatrix _ _ [] = []
checkTransposeMatrix string xs (y:ys)
    | string == "vertical" = horizontal (upper y) xs : checkTransposeMatrix "vertical" xs ys
    | string == "verticalR" = horizontal (upper (reverse y)) xs : checkTransposeMatrix "verticalR" xs ys
 
search :: [String] -> [String] -> [Int]
search xs ys = 
    let h1 = checkMatrix "horizontal" xs ys
        h2 = checkMatrix "horizontalR" xs ys
        h1h2 = [x + y | (x, y) <- h1 `zip` h2]
        h3 = checkTransposeMatrix "vertical" (transpose xs) ys
        h4 = checkTransposeMatrix "verticalR" (transpose xs) ys
        h3h4 = [x + y | (x, y) <- h3 `zip` h4]
    in [x + y | (x, y) <- h1h2 `zip` h3h4]

test1 = search  
        ["ARTTEEB",
         "DSOPNLT",
         "TURNIPA",
         "XKROFET",
         "SKALEOT",
         "MRCTEEB"]
        ["beet", "carrot", "kale", "turnip", "potato"]

test2 = search
        ["ABCBA",
         "BCABC",
         "CABCA",
         "BCABC",
         "ABABA"]
        ["abc", "bca"]

test3 = let line = cycle "dandelion"
            grid = take 400 [take 400 s | s <- tails line]
        in search grid ["Dand", "LION", "iled", "noile", "dandy"]


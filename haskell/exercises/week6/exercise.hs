import Data.Maybe
import Data.Char 
import Data.List hiding (permutations)
{-
2. International Colors
Here are some association lists with colors in a few languages:

cz_colors :: [(String, String)]
cz_colors = [("red", "cerveny"), ("blue", "modry"), ("green", "zeleny")]

fr_colors :: [(String, String)]
fr_colors = [("red", "rouge"), ("blue", "bleu"), ("yellow", "jaune")]

Write a function fr_to_cz :: String -> Just String that maps a color name in 
French to its equivalent name in Czech, by translating through English if 
possible. If the name is absent in either dictionary, return Nothing.
-}
cz_colors :: [(String, String)]
cz_colors = [("red", "cerveny"), ("blue", "modry"), ("green", "zeleny")]

fr_colors :: [(String, String)]
fr_colors = [("red", "rouge"), ("blue", "bleu"), ("yellow", "jaune")]

lookupInFrench :: String -> [(String, String)]-> Maybe String
lookupInFrench _ [] = Nothing
lookupInFrench w (x:list) 
    | snd x == w = Just (fst x) 
    | otherwise = lookupInFrench w list 

fr_to_cz :: String -> Maybe String
fr_to_cz french = do
    e <- lookupInFrench french fr_colors
    c <- lookup e cz_colors
    return (c)

{-
Example -> Given two filenames, read the first file and copy to the second file,
converting all characters to lowercase
-}
convert :: String -> String -> IO ()
convert from to = do
    content <- readFile from
    let updated = map toLower content
    writeFile to updated

{-
3. Merging Files
Write a function merge :: String -> String -> String -> IO () that takes three 
filenames, which are the names of two input files and one output file. The input 
files are sorted files of integers, with one integer per line. The function 
should merge the contents of the files and write them to the output file, 
again with one integer per line. The files may be larger than available memory.
-}

parseToInt :: String -> [Int]
parseToInt = map read . words

merge :: String -> String -> String -> IO()
merge input1 input2 outputFile = do
                content1 <- readFile input1
                content2 <- readFile input2
                let fun (x:content1) (y:content2) = do 
                    case compare x y of
                        LT -> do 
                            writeFile outputFile x
                            writeFile outputFile y
                            fun content1 content2 
                        GT -> do 
                            writeFile outputFile y
                            writeFile outputFile x
                            fun content1 content2 
                        EQ -> do 
                            writeFile outputFile x
                            writeFile outputFile x
                            fun content1 content2
                return ()

{-
4. Permutations
Write a function permutations :: [a] -> [a] that returns a list of all permutations
of a given list. Do not assume that the type a implements Eq.
-}


-- Questions got harder to solve so I will leave it like this 

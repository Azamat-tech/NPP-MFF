import Data.List

wordsPath :: FilePath
wordsPath = "words"

main :: IO()
main = do 
    content <- readFile wordsPath
    let listOfWords = lines content
    mainHelper listOfWords

mainHelper :: [String] -> IO ()
mainHelper [] = return ()
mainHelper (word : listOfWords) = do
        putStrLn "I'm thinking of a word:" 
        let secret = makeDashes word
        putStrLn secret
        game word secret
        putStrLn "Play again?"
        answer <- getLine
        if answer == "yes" 
            then mainHelper listOfWords 
            else return ()
   
game :: [Char] -> String -> IO ()
game word secret = do 
    putStrLn "Your guess?"
    guess <- getLine
    if length guess /= length word then do 
            putStrLn "Wrong number of letters" 
            game word secret
    else if word == guess 
        then do 
            putStrLn "Correct!"
        else do
            putStrLn (checkIfSomeCorrect word guess secret)
            game word secret

charToString :: Char -> String
charToString c = [c]

makeDashes :: [Char] -> String
makeDashes word = intercalate "" (replicate (length word) "-")

revealLetter :: Int -> Char -> String -> String
revealLetter index letter dashes = take index dashes ++ charToString(letter) ++ drop (index + 1) dashes

revealDashes :: Int -> Char -> String -> String -> String
revealDashes _ _ [] dashes = dashes
revealDashes index letter (firstLetter : word) dashes = 
    if letter == firstLetter then 
        let newDashes = revealLetter index letter dashes
        in revealDashes (index + 1) letter word newDashes
    else revealDashes (index + 1) letter word dashes

checkIfSomeCorrect :: String -> String -> String -> String
checkIfSomeCorrect _ [] dashes = dashes
checkIfSomeCorrect word (letter : guess) dashes = 
    if elem letter word then 
        let newDashes = revealDashes 0 letter word dashes
        in checkIfSomeCorrect word guess newDashes
    else checkIfSomeCorrect word guess dashes 

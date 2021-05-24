import Data.List
import Data.Ord

get_set :: (Eq a) => [a] -> [a]
get_set (x:xs) = x : get_set (filter (/= x) xs)
get_set [] = []

get_candidates :: [[String]] -> [String]
get_candidates xs = get_set(concat xs)

get_first_choice :: [[String]] -> [String]
get_first_choice [] = []
get_first_choice (x:xs) = if length x == 0 then get_first_choice xs else head x : get_first_choice xs

not_voted :: [String] -> [String] -> Bool
not_voted [] _ = True
not_voted (x:candidates) votes = if elem x votes then not_voted candidates votes else False

not_voted_candidate :: [String] -> [String] -> String
not_voted_candidate (x : candidates) votes = if elem x votes then not_voted_candidate candidates votes else x

remove_smallest_vote :: [(Int, String)] -> String
remove_smallest_vote xs = snd (minimumBy (comparing fst ) xs)

remove_candidate_from_ballots :: Eq a => a -> [[a]] -> [[a]]
-- remove_candidate_from_ballots _ [] = [[]]
-- remove_candidate_from_ballots candidate (x:xs) = delete candidate x : remove_candidate_from_ballots candidate xs
remove_candidate_from_ballots candidate xs = [filter (/= candidate) l | l <- xs]

voting_eval :: [String] -> [[String]] -> [String]
voting_eval [] _ = []
voting_eval candidates ballots =
    if length candidates == 1 
        then (head candidates) : voting_eval (delete (head candidates) candidates) ballots
        else let first_choice_votes = get_first_choice ballots
             in if not (not_voted candidates first_choice_votes)
                then let eliminate = not_voted_candidate candidates first_choice_votes 
                    in eliminate : voting_eval (delete eliminate candidates) (remove_candidate_from_ballots eliminate ballots)
                else let eliminate = remove_smallest_vote [(length ul, head ul) | ul <- group (sort first_choice_votes)]
                    in eliminate : voting_eval (delete eliminate candidates) (remove_candidate_from_ballots eliminate ballots)


elect :: [[String]] -> [String] 
elect ballots = 
    let candidates = get_candidates ballots
    in voting_eval candidates ballots

test = elect [
  [ "red", "green" ],
  [ "blue" ],
  [ "green", "yellow", "red", "blue" ],
  [ "blue", "green", "yellow", "red" ],
  [ "green" ]
  ]

test2 = elect [
  [ "karel"],
  [ "karel", "petra" ],
  [ "petra" ],
  [ "petra" ],
  [ "karel", "david", "petra" ],
  [ "petra" ],
  [ "lucie", "karel"],
  [ "david", "karel" ],
  [ "david", "petra"],
  [ "david"],
  [ "karel" ],
  [ ],
  [ "petra"]
  ]

test3 = elect [
  [ "purple" ],
  [ "orange", "red", "purple" ],
  [ "blue", "orange" ],
  [ "purple", "orange" ],
  [ "red", "purple", "orange" ],
  [ "red", "blue" ],
  [ "red" ],
  [ "orange" ],
  [ "purple", "red", "blue" ],
  [ "red", "orange", "purple", "blue" ],
  [ "orange", "red", "purple" ],
  [ "red", "blue" ],
  [ "red", "purple" ],
  [ "blue" ],
  [ "purple" ],
  [ "orange", "red" ],
  [ "orange", "purple", "blue", "red" ],
  [ "red" ],
  [ "blue", "orange" ],
  [ "red" ],
  [ "purple", "blue" ],
  [ "purple", "blue" ],
  [ "blue", "red", "orange" ],
  [ "red", "purple", "blue" ]
  ]
-- [ "green" ],
--   [ "blue" ],
--   [ "green", "blue" ],
--   [ "blue", "green" ],
--   [ "green" ]

-- [(2,"blue"),(2,"green"),(1,"red")]
-- 
-- ["red", "blue", "green", "yellow"]
-- ["red", "blue", "green", "blue", "green"]
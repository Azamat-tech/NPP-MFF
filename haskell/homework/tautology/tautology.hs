 import Data.List
import Data.Maybe

data Prop = 
    Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  deriving Show

-- isPart :: [Char] -> Char -> Bool
-- isPart [] _ = True
-- isPart (x:xs) ch = if ch == x then False else isPart xs ch

getVariables :: Prop -> [Char]
getVariables (Const a) = []
getVariables (Var a) = [a]
getVariables (Not a) = getVariables a
getVariables (And a b) = 
    let l1 = getVariables a
        l2 = getVariables b
    in l1 ++ l2

getVariables (Or a b) = 
    let l1 = getVariables a
        l2 = getVariables b
    in l1 ++ l2

truth :: Char -> [(Char, Bool)]
truth v = [(v, b) | b <- [True, False]]

getAllAssignments :: [Char] -> [[(Char, Bool)]]
getAllAssignments [] = [[]]
getAllAssignments (x:variables) = [y:ys | y <- truth x, ys <- getAllAssignments variables]

check :: Prop -> [(Char, Bool)] -> Bool
check (Const a) _ = a
check (Var a) l = fromJust (lookup a l)
check (Not a) l = if check a l == True then False else True
check (And a b) l = 
    let l1 = check a l
        l2 = check b l 
    in if l1 && l2 then True else False

check (Or a b) l = 
    let l1 = check a l
        l2 = check b l 
    in if l1 || l2 then True else False

checkAssingment :: Prop -> [[(Char, Bool)]] -> Bool
checkAssingment _ [] = True
checkAssingment formula (x:listAssignment) = check formula x && checkAssingment formula listAssignment

isTaut :: Prop -> Bool
isTaut formula = 
    let variables = nub(getVariables formula)
        truthAssignments = getAllAssignments variables
    in checkAssingment formula truthAssignments

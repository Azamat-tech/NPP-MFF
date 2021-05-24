import Data.List
import Data.Char
import Data.Maybe


type Vec = (Int, Int)
-- (x, y) in range 1 .. 5
type Pos = (Int, Int)  
-- list of positions of all actors
type State = [Pos]     
-- actor #, start position, end position
type Action = (Int, Vec)

type StatePos = [(Int, Pos)]

type CurrentPos = (Int, Pos)

type StateSpace s a = (s -> [a], s -> a -> s -> s)

dirs :: [Vec]
dirs = [(-1,0), (1,0), (0,1), (0,-1)]

getMultipleLines :: Int -> IO [String]
getMultipleLines n 
    | n == 0 = return []
    | otherwise  = do
        line <- getLine
        otherLines <- getMultipleLines (n-1)
        return (line : otherLines)

main :: IO()
main = do 
    stringMap <- getMultipleLines 5
    let state = toState stringMap 1
    putStrLn (mainHelper (sort state) )

isActor :: Char -> Bool 
isActor x = x /= '-'

toPosition :: String -> Int -> Int -> StatePos
toPosition "-----" _ _ = []
toPosition "" _ _ = []
toPosition (ch : line) row column = 
    if isActor ch then (digitToInt ch, (column, row)) : toPosition line row (column + 1)
                  else toPosition line row (column + 1)


toState :: [String] -> Int -> StatePos
toState [] _ = []
toState (line : otherLines) row = 
    let column = 1
    in toPosition line row column ++ toState otherLines (row + 1)

toString :: [Action] -> String
toString [] = ""
toString ((actor, pos) : otherActions) = 
    case pos of
        (0,-1) -> (show actor ++ "U ") ++ toString otherActions
        (0 ,1) -> (show actor ++ "D ") ++ toString otherActions
        (1, 0) -> (show actor ++ "R ") ++ toString otherActions
        (-1,0) -> (show actor ++ "L ") ++ toString otherActions


mainHelper :: StatePos -> String
mainHelper state = do
    let output = solve (possible, result) state isHatch
    if output == Nothing then "no solution" else
        let withoutJust = fromJust output
        in toString withoutJust

isValid :: Int -> Int -> Int -> Int -> Int -> Bool
isValid dy x currX currY y =
    (dy == 1 && x == currX && currY < (y-1)) || (dy == -1 && x == currX && currY > (y + 1))
    
possible :: StatePos -> [Action]
possible state = 
    let isPossible :: StatePos ->  Vec -> (Int, Pos) -> Bool
        isPossible [] _ _ = False
        isPossible ((actor, (x,y)) : otherStates) (dx,dy) current@(curActor,(currX, currY)) =
            if actor /= curActor then
                if dx == 0 then
                    if isValid dy x currX currY y then True else isPossible otherStates (dx,dy) current
                else
                    if isValid dx y currY currX x then True else isPossible otherStates (dx,dy) current
            else isPossible otherStates (dx, dy) current
    in [(actor,dir) | dir <- dirs, actor <- [0..length state - 1], isPossible state dir (state !! actor) ]

result :: StatePos -> Action -> StatePos-> StatePos
result [] _ _ = []
result ((actorID, (x,y)) : otherStates) (actor, dir) defaultStates = 
    let recurcsiveCall = result otherStates (actor, dir) defaultStates
    in if actorID == actor 
        then performAction defaultStates (actorID, (x,y)) dir : recurcsiveCall
        else (actorID, (x,y)) : recurcsiveCall

noCollision :: StatePos -> Pos -> Bool
noCollision [] _ = True
noCollision ((_ , pos) : otherStates) currPos = 
    if pos == currPos then False else noCollision otherStates currPos

performAction :: StatePos -> CurrentPos -> Vec -> CurrentPos
performAction state (actor, (x,y)) (dx,dy) = 
    if noCollision state (x+dx,y+dy) then performAction state (actor,(x+dx, y+dy)) (dx, dy) else (actor,(x,y))

-- Check if astronaut reached the goal
isHatch :: StatePos -> Bool
isHatch ((id, (x,y)) : _) = x == 3 && y == 3

extend_path :: StateSpace s a -> ([a], s) -> [([a], s)]
extend_path (possible, result) (actions, state) =
    [(a : actions, result state a state) | a <- possible state ]

-- return shortest path from start state to goal state
solve :: Eq s => StateSpace s a -> s -> (s -> Bool) -> Maybe [a]
solve space start isHatch =
    let search explored [] = Nothing
        search explored ((actions, state) : queue)
            | isHatch state = Just actions
            | elem state explored = search explored queue
            | otherwise = search (state : explored) (queue ++ extend_path space (actions, state))
    in reverse <$> search [] [([], start)]
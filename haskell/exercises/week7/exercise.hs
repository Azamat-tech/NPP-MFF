import Data.Maybe
{-
1. Shortest Path
Consider this adjacency-list representation for graphs:

type Graph a = [(a, [a])]
Write a function

shortest_path :: Eq a => Graph a -> a -> a -> Maybe [a]
that takes a graph, a start vertex and end vertex. The function should return a 
list of vertices on the shortest path from the start to the end vertex, or 
Nothing if there is none.
-}
type Graph a = [(a, [a])]

graph1 :: Graph Int
graph1 =
    [(0, [1, 5]), (1, []), (2, [0, 3]), (3, [2, 5]), (4, [2, 3]),
     (5, [4]), (6, [0, 8, 9]), (7, [6, 9]), (8, [6]), (9, [10, 11]),
     (10, [12]), (11, [4, 12]), (12, [9])]

adjacent :: Eq a => Graph a -> a -> [a] 
adjacent graph node = fromJust $ lookup node graph

extend :: Eq a => Graph a -> [a] -> [[a]]
extend graph path@(v : _) = map (: path) (adjacent graph v) 

shortest_path :: Eq a => Graph a -> a -> a -> Maybe [a]
shortest_path graph start goal = 
    let search explored [] = Nothing
        search explored (path@(v : _) : queue) 
            | v == goal = Just path
            | elem v explored = search explored queue
            | otherwise = search (v : explored) (queue ++ extend graph path)
    in reverse <$> search [] [[start]]

{-
2. All Paths
Write a function

all_paths :: Eq a => Graph a -> a -> a -> [[a]]
that takes a graph, a start vertex and end vertex. The function should return 
a list of all possible paths from the start vertex to the end vertex, in 
increasing order of length. No path may contain the same vertex twice.
-}
all_paths :: Eq a => Graph a -> a -> a -> [[a]]
all_paths graph start goal =
    let search explored [] = []
        search explored (path@(v : _) : queue) 
            | v == goal = path : search (v : explored) queue
            | elem v explored = search explored queue
            | otherwise = search (v : explored) (queue ++ extend graph path)
    in reverse <$> search [] [[start]]

{-
3. State Space Search
Write a function

solve :: Eq s => StateSpace s a -> s -> (s -> Bool) -> Maybe [a]
that takes a state space, a start state and a function that determines whether 
a given state is a goal. The function should find the shortest path from the 
start state to any goal state, and should return a list of actions along that 
path. If no goal state can be reached, return Nothing.
-}

type StateSpace s a = (s -> [a], s -> a -> s)

extendSpace :: StateSpace s a -> ([a], s) -> [([a], s)]
extendSpace (possible, result) (actions, state) = 
    [(a : actions, result state a) | a <- possible state]

solve :: Eq s => StateSpace s a -> s -> (s -> Bool) -> Maybe [a]
solve space start isGoal = 
    let search explored [] = Nothing
        search explored (indState@(actions, state) : queue)
            | isGoal state = Just actions
            | elem state explored = search explored queue
            | otherwise = search(state : explored) (queue ++ extendSpace space indState)
    in reverse <$> search [] [([], start)]


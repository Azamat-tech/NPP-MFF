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
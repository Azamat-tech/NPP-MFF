maptree(_, nil).
maptree(P, t(L,X,R)) :- call(P, X), maptree(P, L), maptree(P, R).

maptree(_,nil,nil).
maptree(P, t(L1,X,R1),t(L2,Y,R2)) :- call(P,X,Y),maptree(P,L1,L2),
                                     maptree(P,R1,R2).

restriction(N, H) :- N #=< (2^(H+1))-1.

nodes_in_tree(nil, 0).
nodes_in_tree(t(L,_,R), Nodes) :- Nodes #> 0, LeftNodes #>= 0, RightNodes #>= 0,
                                  Nodes #= 1 + LeftNodes + RightNodes,
                                  nodes_in_tree(L, LeftNodes),nodes_in_tree(R,RightNodes).

height_of_tree(nil, -1).
height_of_tree(t(L,_,R), Height) :- Height #= 1 + max(LeftHeight, RightHeight),
                                    height_of_tree(L, LeftHeight), height_of_tree(R, RightHeight).

size(nil,0,-1).
size(t(nil,_,nil),1,0).
size(T,N,H) :- restriction(N, H), nodes_in_tree(T,N), height_of_tree(T,H).

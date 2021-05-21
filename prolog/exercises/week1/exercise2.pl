last_elem(X, [X]).
last_elem(X, [_|L]) :- last_elem(X,L).

% Write a predicate second_last(X, L) that is true if X is the next-to-last element of L.
second_last(X, [X, _]).
second_last(X,[_|L]) :- second_last(X,L).

% Write a predicate next_to(X, Y, L) that is true if elements X and Y appear in L, with Y immediately after X.
next_to(X,Y,[X,Y|_]).
next_to(X,Y,[_|L]) :- next_to(X,Y,L).
% different version
next_to2(X,Y,L) :- append(_,[X,Y|_], L).

% Write a predicate even_length(L) that is true if the length of L is an even integer.
even_length([]).
even_length([_,_|L]) :- even_length(L).

% Write a predicate same_length(L, M) that is true if L and M have the same length.
same_length([],[]).
same_length([_|L],[_|M]) :- same_length(L,M).

% Write a predicate longer(L, M) that is true if L is longer than M.
longer([_|_],[]).
longer([_|L],[_|M]) :- longer(L,M).

% Write a predicate double_length(L, M) that is true if L is twice as long as M.
double_length([],[]).
double_length([_,_|L],[_|M]) :- double_length(L,M).

% Write a predicate all_same(L) that is true if all elements of L are identical.
all_same([]).
all_same([_]).
all_same([X, X|L]) :- all_same([X|L]).

% Write a predicate reverse(L, M) that is true if L and M contain the same elements in reverse order.
helper1([],H,H).
helper([X|L],H,_) :- helper1(L,[X|H],_).

reverse1(L,M) :- helper1(L,[],M).

% Write a predicate rotate(L, M) that is true if M is L rotated one element to the right (or, equivalently, L is M rotated one element to the left).
rotate([X|L],M) :- append(L, [X], M).

/*
Write a predicate select(X, L, M) that is true if X can be removed from L to
make M. Equivalently, select(X, L, M) is true if X can be inserted anywhere 
in M to make L.
*/
% select(_,[],[]).
select1(X,[X|L],L).
select1(X,[Y|L],[Y|M]) :- select1(X,L,M).     

/*
Write a predicate select(X, L, Y, M) that is true if L and M are identical 
except (possibly) for a single element, which is X in L and is Y in M.
*/
select2(X,L,Y,M) :- select1(X,L,K), select1(Y,M,N), K = N.

% 12. Acyclic Directed Graph
% Write a predicate path(V, W, L) that is true if L is a list of vertices along a path from V to W.
edge(a, b).
edge(a, e).
edge(b, c).
edge(b, f).
edge(e, d).
edge(e, f).
edge(f, c).
edge(g, d).
edge(g, h).
edge(h, f).

path(V,V,[V]).
path(V,W,[V|L]) :- edge(V,Y), path(Y,W,L).

% 13. Edge-List Representation
/*
Write a predicate path(G, V, W, L) that is true if L is a list of vertices 
along a path from V to W in the graph G.
*/
path1(_,V,V,[V]).
path1(G,V,W,[V|L]) :- member([V,X],G), path1(G,X,W,L).

% 14. Cyclic Directed Graph
/*
Write a predicate path(G, V, W, L) that is true if L is a list of vertices
along a path from V to W in G, such that your predicate can always find all 
paths between given vertices V and W.
*/
% generate lists of every possible length
is_list([]).
is_list([_ | L]) :- is_list(L).

% Use iterative deepening to perform a series of depth-first searches for paths
% of every possible length.
path2(G, X, Y, L) :- is_list(L), path1(G, X, Y, L).

% RECODEX Assignment - List a)
% number(one). number(two).
% number(three). number(four).
% number(five).

equals(1, one).
equals(2, two).
equals(3, three).
equals(4, four).
equals(5, five).

numerals([],[]).
numerals([X|L],[S|M]) :- equals(X,S), numerals(L,M).

% b) Write a predicate all_diff(L) that is true if no two elements of L are equal.
comparison(_,[]).
comparison(X, [Y|L]) :- dif(X,Y), comparison(X,L).

all_diff([]).
all_diff([X|L]) :- comparison(X, L), all_diff(L).

% c)Write a predicate flatten(L, M) that is true if L is a list of 
% lists and M is the concatenation of all those lists.
flatten([],[]).
flatten([X|L],Z) :- append(X, Y, Z), flatten(L, Y).
/*
1. Beethoven's Wig - Class Work
Someone has stolen Beethoven's wig and has put it in one of four locked boxes. 
The boxes are numbered 1,2,3,4 in that order. There are four different keys 
that each have their own color. Also:

The green key goes to the third or fourth box.

The wig is to the left of the fourth box.

The wig is to the right of the first box.

The yellow key is to the left of the wig.

The blue key is to the right of the yellow key and to the left of the green key.

The red key goes to the first box.

Which box contains Beethoven's wig? Write a Prolog program that can find the answer.
*/
solve(Red,Green,Yellow,Blue,Wig) :-
    Keys = [Red,Green,Yellow,Blue],
    all_distinct(Keys), % or dif(EACH OF THE COLORS)
    Keys ins 1..4,
    Green in 3..4,
    Wig in 2..3,
    Yellow #< Wig,
    Blue #> Yellow, Blue #< Green,
    Red = 1.

/*
2. Cryptarithmetic - Class Work
In this cryptarithmetic puzzle, every letter stands for a different digit:

  A P P L E
+ G R A P E
+   P L U M
=========== 
B A N A N A
Additionally, no leading digit may be zero.

Write a Prolog program that can find a solution to the puzzle.
*/
helper(X, Old, New) :- New #= 10 * Old + X.

as_number(L, N) :- foldl(helper, L, 0, N).

solveC(Apple, Grape, Plum, Banana) :- 
    Apple = [A,P,P,L,E],
    Grape = [G,R,A,P,E],
    Plum = [P,L,U,M],
    Banana = [B,A,N,A,N,A],
    Digits = [A,B,E,G,L,M,N,P,R,U],
    all_distinct(Digits),
    Digits ins 0..9,
    maplist(as_number, [Apple, Grape, Plum, Banana], [Ap, Gr, Pl, Ba]),
    maplist(#\=(0), [A, G, P, B]),
    Ap + Gr + Pl #= Ba,
    labeling([bisect], Digits).

/*
3. N Queens - Class Work
Can we place N queens on a chessboard so that none of them attack any other?

Write a predicate queens(N, L) that is true if L is a solution to the N-queens
problem. L will be a list of integers from 1 to N representing the row position
of the queen in each column. Your predicate should be able to check and generate solutions.
*/
% True if a queen at (X1, Y1) does not attack a queen at (X2, Y2).
no_attack(X1 / Y1, X2 / Y2) :-
    X1 #\= X2, Y1 #\= Y2, abs(X1 - X2) #\= abs(Y1 - Y2).

% True if none of the positions in L attack any other position.
% Each position is of the form X / Y.
none_attack([]).
none_attack([P | L]) :- maplist(no_attack(P), L), none_attack(L).

% zip([a, b, c], [d, e, f], [a / d, b / e, c / f])
pair(X, Y, X / Y).
zip(L, M, Z) :- maplist(pair, L, M, Z).

queens(N, L) :-
    length(L, N),
    L ins 1 .. N,
    all_distinct(L),
    numlist(1, N, Nums), zip(Nums, L, Positions),
    none_attack(Positions),
    label(L).

/*
4. Magic Square
A magic square is a square of size N x N that contains all integers from 1 .. N2
and in which every row, column and diagonal adds to the same value.

Write a Prolog predicate that can test whether a square is magical. When run 
backward, it should also be able to generate magic squares of a given size. 
*/

% permutation
takeout(X,[X|R],R).  
takeout(X,[F |R],[F|S]) :- takeout(X,R,S).

permutation_([],[]).
permutation_([X|L],M) :- permutation_(L,Z), takeout(X,M,Z).

% the transpose of a matrix
transpose1([], []).
transpose1([[H|T] |Tail], [[H|NT] |NTail]) :- 
	firstCol(Tail, NT, Rest), transpose1(Rest, NRest), firstCol(NTail, T, NRest).

firstCol([[H|T] |Tail], [H|Col], [T|Rows]) :- firstCol(Tail, Col, Rows).
firstCol([], [], []).

%Create matrix
lengthing(N,L) :- length(L,N).

get_matrix(N,M) :- length(M,N), maplist(lengthing(N), M).

% Check the row of the sum
addition(X,Y,Z) :- X + Y #= Z.

summation(L,SD) :- SD #= K, foldl(addition, L, 0, K).

sum_row_check([],_).
sum_row_check([R|M],SD) :-
    summation(R,SD),
    sum_row_check(M,SD).
    
% Get the diagonal 
diagonal1([],_,_,[]).
diagonal1([R|M],N,P,[X|LD]) :-
    nth1(N,R,X),
    N1 #= N + P,
    diagonal1(M,N1,P,LD).

% Magic Square predicate
magical(N,M) :-
    N > 0, N1 is N^2, SD is N*(N*N+1)/2,
    get_matrix(N,M), flatten(M, Vars),
    Vars ins 1..N1, sum_row_check(M,SD),
    transpose1(M,Tmatrix),
    sum_row_check(Tmatrix,SD),
    diagonal1(M,N,-1,D1),
    summation(D1,SD),
    diagonal1(M,1,1,D2),
    summation(D2,SD),
    all_distinct(Vars),
    label(Vars).

% 5-6 Skippped

/*
7. Tree Check
In Prolog we can represent a binary tree as either the atom nil (for the 
empty tree) or as t(L, V, R), where V is the value at the root and L and R
are the left and right subtrees.

Write a Prolog predicate isTree(T) that is true if T is a binary tree. 
(It does not need to be a binary search tree; values can appear in any 
order and can have any type).
*/

isTree(nil).
isTree(t(L,_,R)) :- isTree(L), isTree(R).

/*
8. Tree Sum
Write a predicate sum(T, N) that is true if T is a binary tree of 
integers and N is the sum of the integers in the tree.
*/
sumT(nil,0).
sumT(t(L,V,R),S) :- S #= V + S1 + S2, sumT(L,S1), sumT(R,S2).

/*
9. Tree Leaves
Write a Prolog predicate leaves(T, S) that is true if S is a list of
values in the leaves of the binary tree T. Recall that a leaf is a 
node with no chlidren. Ideally your predicate will run in time O(|S|). 
Your predicate should work in both directions.
*/

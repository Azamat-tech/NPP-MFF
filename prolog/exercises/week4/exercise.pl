/*
Bonus - Bubble Sort
*/

pass_b([],[]).
pass_b([X],[X]).
pass_b([X,Y|L],M) :- pass_b([Y|L],[Z|R]), 
                (X #< Z, M = [X,Z|R]; X #>= Z, M = [Z,X|R]).

bubble_sort([],[]).
bubble_sort(L,[X|S]) :- pass_b(L,[X|R]), bubble_sort(R,S).

/*
1. Insertion Sort
Write a predicate that sorts a list of integers. Use an insertion sort.
*/
pass_i(X,[],[X]).
pass_i(X,[Y|L],[X,Y|L]) :- X #=< Y.
pass_i(X,[Y|L],[Y|Z]) :- X #> Y, pass_i(X,L,Z).

insertion_sort([],[]).
insertion_sort([X|L],M) :- insertion_sort(L,R), pass_i(X,R,M).

/*
2. Permutations
Write a predicate permutation(L, M) that is true if the list L is a permutation
of M.
*/
takeout(X,[X|R],R).  
takeout(X,[F |R],[F|S]) :- takeout(X,R,S).

permutation_([],[]).
permutation_([X|L],M) :- permutation_(L,Z), takeout(X,M,Z).

/*
3. Combinations
Write a predicate combination(L, N, M) that is true if M is a combination of N
elements of L, i.e. a subset of size N that has its elements in the same order as in L.
*/
combination(_,0,[]).
combination([X|L],N,[X|M]) :- N #> 0, N1 #= N - 1, combination(L,N1,M).
combination([_|L],N,M) :- N #> 0, combination(L,N,M).

/*
4. Merge Sort
Write a predicate that sorts a list of integers. Use a merge sort.
*/
divide([],[],[]).
divide([X|L],L1,[X|L2]) :- divide(L,L2,L1).

merge([],L2,L2).
merge(L1,[],L1).
merge([X|L1],[Y|L2],[X|S]) :- X #=< Y, merge(L1,[Y|L2],S).
merge([X|L1],[Y|L2],[Y|S]) :- Y #< X, merge([X|L1],L2,S).

mergesort([],[]).
mergesort([X],[X]).
mergesort(L, S) :- length(L,N), N #> 1, divide(L, L1,L2), mergesort(L1,L1R), mergesort(L2,L2R),
                   merge(L1R,L2R,S).

/*
5. Quicksort
Write a predicate that sorts a list of integers. Use a quicksort.
*/
quicksort([],[]).
quicksort([X|L],M) :- 
        pivoting(X,L,L1,L2), quicksort(L1,L1R), 
        quicksort(L2,L2R), append(L1R,[X|L2R],M).
   
pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G) :- X #=< H, pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]) :- X #> H, pivoting(H,T,L,G).

/*
6. Digits
Write a predicate digits(L, N) that converts a list of digits (e.g. [3, 4, 5])
to an integer (e.g. 345).
*/
conc(X,A,NX) :- NX #= X + A * 10.
digits(L,M) :- foldl(conc, L, 0, M).

/*
7. Vectors
We can represent a vector as a list of floating-point numbers. Write predicates that can calculate the following:

the sum of two vectors

multiplying a scalar by a vector

the dot product of two vectors

the angle between two vectors

All predicates should work in any direction.
*/
add(X,Y,Z) :- {X + Y = Z}.
sum_v(V,W,Z) :- maplist(add,V,W,Z).

multiply(X,Y,Z) :- {X * Y = Z}.
mult_scalar(X,L,Z) :- maplist(multiply(X),L,Z).

add_d(X,A,R) :- {X + A = R}.
dot_product(V,W,Z) :- maplist(multiply,V,W,T), foldl(add_d,T,0,Z).

addS(X,A,S) :- X1 is X^2, S is X1 + A. 

magnitude(V,R) :- foldl(addS,V,0,S), R is sqrt(S).

angle_between(V,W,A) :- 
            dot_product(V,W,D), magnitude(V,M1),magnitude(W,M2),
            M is M1 * M2, R is D / M, A is acos(R).


/*
8. Matrices
We can represent a matrix as a list of lists of floating-point numbers.

Write a predicate that tests whether a matrix has dimensions N x N, for a given N.
Also write predicates that can calculate the following:

All predicates should work in any direction.
*/
% 1) the zero matrix of a given size
lengthing(N,L) :- length(L,N).

maplist(_, []).
maplist(C,[X|Xs]) :-
   call(C,X),
   maplist(C, Xs).

zerosL(L) :- maplist(=(0),L).

maplist_(_,[]).
maplist_(P,[X|Xs]) :-
        call(P,X),
        maplist_(P,Xs).

zero_matrix(N, M) :- length(M,N), maplist(lengthing(N), M), maplist_(zerosL, M).

addLists(E1,E2,E3) :- { E1 + E2 = E3 }.
addMatrices(L1,L2,L3) :- maplist(addLists,L1,L2,L3).
sum_matrices(M1,M2,M3) :- 
                get_dimension(M1,N1), get_dimension(M2,N2),
                N1 #= N2, maplist(addMatrices,M1,M2,M3).

% 3) the first column of a matrix (a vector)
get_first_column(M,V) :- maplist(nth0(1),M,V).

% 4) the identity matrix of a given size
zeros(0,[]).
zeros(N,[0|L]) :- succ(N1,N), zeros(N1,L).

replaceH(_,[],[],_).
replaceH(I,[_|L],[1|R],J) :- I #= J, J1 #= J + 1, replaceH(I,L,R,J1).
replaceH(I,[X|L],[X|R],J) :- I #\= J, J1 #= J + 1, replaceH(I,L,R,J1).

replace(I, L, R) :- replaceH(I,L,R,1).

id_list(P,S,L) :- length(L1,S), maplist(=(0),L1), replace(P,L1,L).

restrict_size(_,[]).
restrict_size(N,[X|M]) :- length(X,N), restrict_size(N,M).

identityH(X,X,L1) :- id_list(X,X,L1).
identityH(N,I,M) :- 
                N #\= I, id_list(I,N,L), I1 #= I + 1,
                identityH(N,I1,M1), append(L,M1,M).

identity(N,M) :- length(M,N), identityH(N,1,R), append(M,R), restrict_size(N,M).

% 5) the transpose of a matrix
transpose1([], []).
transpose1([[H|T] |Tail], [[H|NT] |NTail]) :- 
	firstCol(Tail, NT, Rest), transpose1(Rest, NRest), firstCol(NTail, T, NRest).

firstCol([[H|T] |Tail], [H|Col], [T|Rows]) :- firstCol(Tail, Col, Rows).
firstCol([], [], []).

% 6) the product of two matrices
row_multiplication(T,M1,M3) :- maplist(dot_product(M1),T,M3).

multiplyM(M1,M2,M3) :- transpose1(M2,T), maplist(row_multiplication(T),M1,M3).


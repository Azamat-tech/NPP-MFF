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


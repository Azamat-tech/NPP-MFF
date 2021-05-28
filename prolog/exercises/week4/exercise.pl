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
Write a predicate permutation(L, M) that is true if the list L is a permutation of M.
*/
takeout(X,[X|R],R).  
takeout(X,[F |R],[F|S]) :- takeout(X,R,S).

permutation_([],[]).
permutation_([X|L],M) :- permutation_(L,Z), takeout(X,M,Z).
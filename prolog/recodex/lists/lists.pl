equals(1, one).
equals(2, two).
equals(3, three).
equals(4, four).
equals(5, five).

numerals([], []).
numerals([X | L], [Y | M]) :- equals(X,Y), numerals(L, M).

different(_,[]).
different(X,[Y|L]) :- dif(X,Y), different(X,L).

all_diff([X,Y]) :- dif(X,Y).
all_diff([X | L]) :- different(X, L), all_diff(L).

flatten(L, M) :- append(L, M).

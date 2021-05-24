dup([],[]).
dup([X|L], [X,X|M]) :- dup(L, M).

dedup([X],[X]).
dedup([X,X|L],[X|M]) :- dedup([X|L],[X|M]).
dedup([X,Y|L],[X|M]) :- dif(X,Y), dedup([Y|L],M).

group([X],[[X]]).
group([X,X|L],[[X|N]|M]) :- group([X|L],[N|M]).
group([X,Y|L],[[X]|M]) :- dif(X,Y), group([Y|L], M).


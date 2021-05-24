person(david).
person(thomas).
person(emma).
person(stella).

male(david).
male(thomas).

female(emma).
female(stella).

different(P,Q,R,S) :-
    person(P), person(Q), person(R), person(S),
    dif(P,Q), dif(P,R), dif(P,S),
    dif(Q,R), dif(Q,S), dif(R,S).

sit_across(X, Y) :- (X = emma, Y = stella);(Y = emma, X = stella);
               (X = david, Y = thomas);(Y = david, X = thomas).

solve(Dumplings, Pasta, Soup, Trout) :-
    different(Dumplings, Pasta, Soup, Trout),
    different(Cider, Beer, Wine, Icedtea),
    dif(Cider, Trout), sit_across(Cider, Trout),
    Dumplings = Beer,
    Soup = Cider,
    dif(Pasta, Beer), sit_across(Pasta, Beer),
    dif(david, Icedtea),
    Wine = emma,
    dif(stella, Dumplings).

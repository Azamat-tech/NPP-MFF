grand_parent(G, X) :- parent(Y, X), parent(G, Y).

% G is a great grand mother of X
great_grandmother(G, X) :- grand_parent(Y, X), female(G), parent(G, Y).

% Two people are siblings if they have at least one parent in common
sibling(X, Y) :- parent(P, X), parent(P, Y), dif(X, Y).

% Full siblings share both parents.
full_sibling(X, Y) :- (parent(D, X), parent(D, Y), male(D)), (parent(M, X), parent(M, Y), female(M)), dif(X, Y).

% First cousings have parents who are full siblings.
first_cousin(X, Y) :- parent(P, X), full_sibling(P, Z), parent(Z, Y).

% Second cousins have parents who are filrst cousins.
second_cousin(X, Y) :- parent(P, X), first_cousin(P, Z), parent(Z, Y).

% True if X and Y are N-th cousins for any N>=1.
nth_cousin(X, Y) :- first_cousin(X, Y).

nth_cousin(X, Y) :- parent(P, X), nth_cousin(P, Z), parent(Z, Y).

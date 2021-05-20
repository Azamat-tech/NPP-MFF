helper(X, Y, Z) :- (X = zero, Y = zero, Z = zero);
                   (X = one , Y = one, Z = zero);
                   (X = one, Y = zero, Z = one);
                   (X = zero, Y = one, Z = one).

carry(X, Y, Z) :- (X = zero, Y = zero, Z = zero);
                  (X = one, Y = one, Z = one);
                  (X = one, Y = zero, Z = zero);
                  (X = zero, Y = one, Z = zero).

helper2(X, Y, Z, C1) :- (helper(X, Y, Z), C1 = zero);
                        (X = one, Y = zero, Z = zero, C1 = one);
                        (X = zero, Y = one, Z = zero, C1 = one);
                        (X = zero, Y = zero, Z = one, C1 = one);
                        (X = one, Y = one, Z = one, C1 = one).

carry2(C2, X, Y, C1) :- (C2 = one, X = one, Y = one);
                        (C2 = zero, X = zero, Y = zero, C1 = one);
                        (carry(X, Y, C2), C1 = zero);
                        (C2 = one, X = one, Y = zero, C1 = one);
                        (C2 = one, X = zero, Y = one, C1 = one).

add(X3, X2, X1, X0, Y3, Y2, Y1, Y0, Z4, Z3, Z2, Z1, Z0) :-
    helper(X0, Y0, Z0),
    carry(X0, Y0, Carry1),
    helper2(X1, Y1, Z1, Carry1),
    carry2(Carry2, X1, Y1, Carry1),
    helper2(X2, Y2, Z2, Carry2),
    carry2(Carry3, X2, Y2, Carry2),
    helper2(X3, Y3, Z3, Carry3),
    carry2(Carry4, X3,Y3, Carry3),
    Z4 = Carry4.

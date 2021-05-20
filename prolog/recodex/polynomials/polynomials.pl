add_p([], Q, Q).
add_p(P, [], P).
add_p([X|P], [Y|Q], [Z|R]) :- Z #= X + Y, add_p(P, Q, R).

sum_p([],_,_, S, S).
sum_p([X | L], V, C, A, R) :- A1 #= A + X * V^C, C1 #= C + 1,
                              sum_p(L, V, C1, A1, R).

eval(P, X, Y) :- sum_p(P, X, 0, 0, Y).

length_le([], _).
length_le([_ | L], [_ | M]) :- length_le(L, M).

scalar([], _, []).
scalar([X|P], S, [Z|R]) :- Z #= X * S, scalar(P, S, R).

mul_p([], _, []).
mul_p([X | P], Q, R) :- length_le(P, Q), mul_p(P, Q, K),
                        scalar(Q, X, RP), add_p(RP, [0 | K], R).

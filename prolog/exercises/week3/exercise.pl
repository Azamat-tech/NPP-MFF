/*
1. Reverse
Write a predicate reverse(L, M) that is true if M is the reverse of the list L.
*/
reverse1([],[]).
reverse1([X|L],M) :- reverse(L,R), append(R,[X],M).

/*
2. Addition
Suppose that we represent natural numbers using structures: 0 = z, 1 = s(z), 
2 = s(s(z)) and so on. Write a predicate sum(I, J, K) that is true if I + J = K, 
where I, J, and K have this numeric representation. Your predicate should work in all directions.
*/

% sum(I,J,K) :- 

/*
3. Multiplication
Extending the previous exercise, write a predicate mul(I, J, K) that is true if I · J = K, 
where I, J, and K have this numeric representation.Your predicate should work in all directions.
*/

/*
4. Integer Sum
Write a predicate sum(L, N) that is true if N is the sum of the integers in the list L.
*/
sum1([], 0).
sum1([X|L],R) :- X+Y#=R, sum1(L,Y). 

/*
5. Real Sum
Write a predicate sum(L, R) that is true if R is the sum of the floating-point 
numbers in the list L.
*/
sum2([],0).
sum2([X|L],R) :- {X+Y=R}, sum2(L,Y).

/*
6. Ordered List
Write a predicate ordered(L) that is true if the floating-point numbers in L are 
in non-decreasing order.
*/
ordered([]).
ordered([_]).
ordered([X,Y|L]) :- {X < Y}, ordered([Y|L]).

/*
7. Time Difference
Let the structure time(H, M, S) represent the 24-hour time H:M:S, where H, M, 
and S are integers. Write a predicate plus(T, N, Q) that is true if time T plus 
N seconds equals time Q. Your predicate should work in all directions.
*/
convert(time(H,M,S), N) :-
    [M,S] ins 0..59, H in 0..23, N #= 3600 * H + 60 * M + S.

add(T1, N, T2) :- convert(T1, N1), convert(T2,N2), N1 + N #= N2.

/*
8. Factorial
Write a predicate factorial(N, F) that is true if F = N! . Your predicate
should work in all directions and should terminate when the solution set is finite.
*/
factorial(0,1).
factorial(N,F) :- N #> 0, N*R#=F, N1#=N-1, 
    factorial(N1,R).

/*
9. Sum of First N Elements
Write a predicate sum_n(N, L, S) that is true if S is the sum of the first N 
elements of L. If L has fewer than N elements, the predicate should fail.
*/
sum3(0,_,0).
sum3(N,[X|L],S) :- N #> 0, N1 #= N - 1, S #= X + S1, sum3(N1,L,S1).

/*
10. Slice
Write a predicate slice(L, I, J, M) that is true if M contains elements I .. J
of L, where elements are indexed starting from 1. For example, slice([r, i, v, e, r],
2, 4, [i, v, e]) is true.
*/

slice([X|_],1,2,[X]).
slice([],_,_,[]).
slice([X|L],1,T,[X|M]) :- T > 1, T1 #= T - 1, slice(L,1,T1,M).
slice([_|L],F,T,M) :- F > 1, F1 #= F-1, slice(L,F1,T,M).


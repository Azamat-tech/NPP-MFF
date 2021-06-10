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

sum0(z,A,A).
sum0(s(A),B, s(C)) :- sum0(A, B, C).
 
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
slice([X|L],1,T,[X|M]) :- T #> 1, T1 #= T - 1, slice(L,1,T1,M).
slice([_|L],F,T,M) :- F > 1, F1 #= F-1, slice(L,F1,T,M).

/*
11. Day Count
Write a predicate total_days(Y, Z, N) that is true if N is the total number of 
days in the years Y .. Z. Assume that 1900 < Y, Z < 2100 (which makes leap year calculations easier).
*/
% N is 1 if Year is a leap year, otherwise 0.
is_leap(Year, N) :- N #<==> Year mod 400 #= 0 #\/ Year mod 4 #= 0 #/\ Year mod 100 #\= 0.

calculate(Y,Y,0).
calculate(Y,Z,N) :- dif(Y,Z), Y1 #= Y + 1,
    ((is_leap(Y, T), T #= 1, N #= 366 + N1, N1 #>= 0, calculate(Y1,Z,N1));
    ( is_leap(Y, T), T #= 0, N #= 365 + N1, N1 #>= 0, calculate(Y1,Z,N1))).

total_days(Y,Z,N) :- 1900 < Y, Z < 2100, calculate(Y,Z,N).

% 12 skipped

/*
13. Greatest Common Divisor
Write a predicate gcd(I, J, K) that is true if the greatest common divisor of I and J is K.
*/
gcd(I,0,I).
gcd(I,J,K) :- J1 #= I mod J, gcd(J,J1,K).

/*
14. Prime
Write a predicate is_prime(N) that is true if N is prime.
*/
has_factor(N,K) :- N mod K #= 0.
has_factor(N,K) :- K * K #< N, K1 #= K + 2, has_factor(N,K1).

is_prime(2).
is_prime(3).
is_prime(N) :-N #> 3, N mod 2 #\= 0, \+ has_factor(N, 3).

/*
15. All Primes
Write a predicate all_primes(I, J) that returns a list of all prime numbers between I and J, inclusive.
*/
next(A,A1) :- A1 #= A + 1.

all_primes(I,J,[]) :- I #> J, !.
all_primes(I,J,[I|L]) :- is_prime(I), !, next(I,K), all_primes(K,J,L).
all_primes(I,J,L) :- next(I,K), all_primes(K,J,L).

/*
16. Smallest Prime Factor
Write a predicate smallest_factor(N, P) that is true if P is the smallest prime factor of N.
*/
smallest_factor(N,P) :- N #> 0, list_prime(N,K,2), nth0(0,K,P).

list_prime(1,[],_) :- !.
list_prime(N,[K|L],K) :- R #= N // K, N #= K * R, !, list_prime(R,L,K).
list_prime(N,L,K) :- next(N,K,NK), list_prime(N,L,NK).

next(N,F,NF) :- F * F #< N, !, NF #= F + 1.
next(N,_,N).

/*
17. Drop Every Nth
Write a predicate drop(L, N, M) that is true if we can remove every Nth element from L to make M. 
For example, drop([a, b, c, d, e, f, g], 3, [a, b, d, e, g]) is true.
*/
drop(L,N,M) :- drop(L,N,M,N).
drop([],_,[],_).
drop([X|L],K,[X|M],N) :- K #\= 1, K1 #= K - 1, drop(L,K1,M,N).
drop([_|L],1,M,N) :- drop(L,N,M,N).

/*
18. Number of Prime Factors
Write a predicate num_factors(A, N) that is true if A has exactly N prime factors, where repeated
factors are counted separately. For example, num_factors(12, 3) is true since 12 = 2 x 2 x 3.
*/
num_factors(A,N) :- list_prime(A,L,2), length(L,N).

% RECODEX HOMEWORKS
% More Lists
/*
Write a predicate zip(?L, ?M) that is true if L is a list of pairs and M is a corresponding pair of 
lists, where we represent pairs using the structure X : Y.
*/
zip([],[]:[]).
zip([X:Y|L],[X|M]:[Y|Z]) :- zip(L,M:Z).

/*
b) Write a predicate multidup(?L, +N, ?M) that is true if M can be formed by duplicating each element
in L N times. Your predicate should terminate in both directions if the solution set is finite.
*/

duplicate(_,0,[]).
duplicate(X,N,NL) :- N #> 0, N1 #= N - 1, append([X],R,NL), duplicate(X,N1,R).

multidup([],_,[]).
multidup([X|L],N,M) :- duplicate(X,N,NL), append(NL,R,M), multidup(L,N,R).

/*
c) Write a Prolog predicate rotate(?L, ?M) that is true if L can be rotated by any number of positions
(including zero) to form M.
*/
rotate(L,M) :- same_length(L,M), append(X,Y,L), append(Y,X,M).
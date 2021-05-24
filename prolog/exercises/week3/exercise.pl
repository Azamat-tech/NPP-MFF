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

sum(I,J,K)


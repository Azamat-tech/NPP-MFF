% 1) Mortal Men 
man(socrates).
man(plato).

mortal(X) :- man(X).

% 2) Single Clause
parent2(charles, X) :- X = wenceslause; X = margaret; X = sigismund.

parent1(X, Y) :- X = charles, (Y = wenceslause; Y = margaret; Y = sigismund); X = sigismund, Y = elizabeth.

% 3) Dancing Pairs
male1(hans).
male1(charles).

female1(elizabeth).
female1(kate).

dance(P, Q) :- male(P), female(Q).

% 4) Drinking Pairs
drinks(thomas, whiskey).
drinks(sonya, beer).
drinks(lucy, wine).
drinks(radek, beer).
drinks(jarda, beer).

pair(P, Q, D) :- drinks(P, D), drinks(Q, D).

% 5) Directed Graph
edge(a, b).
edge(a, e).
edge(b, c).
edge(b, f).
edge(e, d).
edge(e, f).
edge(f, c).
edge(g, d).
edge(g, h).
edge(h, f).

%)6 a) Write a predicate path(V, W) that is true if there is some path from V to W in the directed graph.
path(V, W) :- edge(V,W).
path(V, W) :- edge(V, Z), path(Z, W).

% Family Tree - Recodex Exercise 
male(liam). male(noah). male(oliver). male(william).
male(elijah). male(james). male(ben).

female(olivia). female(emma). female(ava).
female(sophia). female(isabella). female(evelyn).

parent(liam, oliver).
parent(olivia, oliver).

parent(liam, emma).
parent(olivia, emma).

parent(noah, william).
parent(olivia, william).

parent(william, evelyn).

parent(oliver, ava).
parent(ava, james).
parent(james, isabella).

parent(emma, elijah).
parent(elijah, sophia).
parent(sophia, ben).

%True if G is the great-grandmother of X.
great_grandmother(G,X) :- female(G), parent(G, Y), parent(Y, Z), parent(Z, X).
%Two people are siblings if they have at least one parent in common.
sibling(X,Y) :-  dif(X,Y), parent(Z, X), parent(Z, Y).
%Full siblings share both parents.
full_sibling(X,Y) :- dif(X,Y), (parent(Z,X), parent(Z,Y), male(Z)), (parent(W,X), parent(W,Y), female(W)).
%First cousins have parents who are full siblings.
first_cousin(X,Y) :- dif(X,Y), parent(Z,X), full_sibling(Z, W), parent(W,Y).
%Second cousins have parents who are first cousins.
second_cousin(X,Y) :- dif(X,Y), parent(Z,X), first_cousin(Z,W), parent(W,Y).
%True if X and Y are Nth cousins for any N ≥ 1.
nth_cousing(X,Y) :- first_cousin(X,Y).
nth_cousing(X,Y) :- dif(X,Y), parent(Z,X), nth_cousing(Z,W), parent(W,Y).

% 7 Map of Europe
    %Is it possible to color this map with 3 colors so that no bordering countries have the same color?
    %Write a Prolog program that can answer this question.
color(red). 
color(blue).
color(green).
% color(yellow). 

solve1(Austria, Czech, Germany, Hungary, Poland, Slovakia, Ukraine) :-
    color(Czech), color(Slovakia), color(Germany), color(Poland),
    color(Austria), color(Hungary), color(Ukraine),
    dif(Czech, Germany), dif(Czech, Poland), dif(Czech, Slovakia), dif(Czech, Austria),
    dif(Slovakia, Poland), dif(Slovakia, Ukraine), dif(Slovakia, Hungary),
    dif(Germany, Poland), dif(Germany, Austria),
    dif(Poland, Ukraine),
    dif(Austria, Hungary),
    dif(Hungary, Ukraine).

% 8 Crossword
/*
Suppose that we'd like to fill in a 3 x 3 grid with letters so that every row and column contains one of these words:
AGE, AGO, CAN, CAR, NEW, RAN, ROW, WON
Write a Prolog program that can find all possible solutions.
*/
word(a,g,e).
word(a,g,o).
word(c, a, n).
word(c, a, r).
word(n, e, w).
word(r, a, n).
word(r, o, w).
word(w, o, n).

solve2(A,B,C,D,E,F,G,H,I) :-
    word(A,B,C), word(D,E,F), word(G,H,I),
    word(A,D,G), word(B,E,H), word(C,F,I).
    
% 9) Mini-MineSweeper
one(mine, no).
one(no, mine).
two(mine, mine, no).
two(no, mine, mine).
two(mine, no, mine).

solve3(A,B,C,D,E) :-
    one(A,B),
    two(A,B,C),
    two(B,C,D),
    one(D,E).

% 10) Occupation and instruments
/*
Write a Prolog program that can solve the following puzzle.
Kate, Maria and Roman have distinct occupations and play distinct musical instruments. Their occupations are doctor, lawyer, and teacher, and the instruments they play are piano, flute, and violin. Also:
Maria lives next to the doctor.
The lawyer plays the piano.
Maria is not the teacher.
Kate is a patient of the violinist.
Who plays the flute?
*/
person(kate).
person(maria).
person(roman).

different(X,Y,Z) :-
    person(X), person(Y), person(Z),
    dif(X,Y), dif(Y,Z), dif(X,Z).

solve4(Piano, Flute, Violin, Doctor, Lawyer, Teacher) :-
    different(Piano, Flute, Violin),
    different(Doctor, Lawyer, Teacher),
    dif(maria, Doctor),
    Lawyer = Piano,
    dif(maria, Teacher),
    Violin = Doctor,
    dif(kate, Doctor).

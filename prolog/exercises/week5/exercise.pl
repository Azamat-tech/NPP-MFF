% STACK
% S is an empty stack.
empty_stack(stack([])). 

% We can push X onto S to make the stack T.
push(X, stack(S), stack([X | S])).

% We can pop X from S to make the stack T.
pop(X, stack([X | S]), stack(S)).
% S is the empty stack with the numbers 1 .. N pushed onto it.
push_to_n(0, S) :- empty_stack(S).
push_to_n(N, S) :- N #> 0, N1 #= N - 1, push_to_n(N1, S1), push(N, S1, S).

% QUEUE 
% back / front stacks
empty_queue([] / []).

% enqueue(X, Q, R) enqueues X onto Q to make R, pushing X to the back stack.
enqueue(X, L / M, [X | L] / M).

% dequeue(X, Q, R) dequeues X from Q to make R.  It pops from the front stack
% if it's non-empty, otherwise reverses the back stack and moves it to the front.
dequeue(X, L / [X | M], L / M).
dequeue(X, L / [], [] / M) :- reverse(L, [X | M]).
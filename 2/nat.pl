% Natural numbers

% nat(X)
nat(zero).
nat(suc(N)) :-
    nat(N).

% add(X, Y, Z) iff X + Y = Z
add(zero, M, M).
add(suc(N), M, suc(R)) :-
    add(N, M, R).

% subtract(X, Y, Z) iff X - Y = Z
subtract(N, zero, N).
subtract(suc(N), suc(M), R) :-
    subtract(N, M, R).

% mul(X, Y, Z) iff X * Y = Z
mul(zero, _, zero).
mul(suc(N), M, R) :- 
    mul(N, M, T),
    add(T, M, R).

% lt(X, Y) iff X < Y
lt(zero, suc(_)).
lt(suc(N), suc(M)) :-
    lt(N, M).

% fib(Idx, A, B) iff Fib_Idx = A, Fib_{Idx + 1} = B
fib(zero, zero, suc(zero)).
fib(suc(Idx), Cur, Next) :-
    fib(Idx, Prev, Cur),
    add(Prev, Cur, Next).

% fib(Idx, A) iff Fib_Idx = A.
fib(Idx, A) :-
    fib(Idx, A, _).

fib2(zero, zero)).
fib2(suc(zero), suc(zero)).
fib2(suc(suc(N)), R) :- 
    fib2(suc(N), A),
    fib2(N, B),
    add(A, B, R).

% exp(Base, Exp, Result).
exp(_Base, zero, suc(zero)).
exp(Base, suc(Exp), Result) :-
    exp(Base, Exp, Temp),
    mul(Temp, Base, Result).

% modulo(X, Y, Z).
modulo(X, Y, X) :- 
    lt(X, Y).
modulo(X, Y, Z) :-
    subtract(X, Y, Xp),
    modulo(Xp, Y, Z).

% gcd(X, Y, Z).
gcd(zero, X, X).
gcd(suc(X), Y, Z) :-
    modulo(Y, suc(X), M),
    gcd(M, suc(X), Z).

% factorial(N, Fact).
factorial(zero, suc(zero)).
factorial(suc(N), R) :-
    factorial(N, Temp),
    mul(suc(N), Temp, R).

display(zero, 0).
display(suc(N), W) :-
    display(N, U),
    W is (U + 1).

show(0, zero).
show(W, suc(N)) :-
    W > 0,
    U is (W - 1),
    show(U, N).

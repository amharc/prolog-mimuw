% dlugosc

dlugosc([], 0).
dlugosc([_|T], s(X)) :- dlugosc(T, X).

dlugosc2(L, Res) :- dlugosc2(L, 0, Res).
dlugosc2([], Res, Res).
dlugosc2([_|T], Prev, Res) :- Next is Prev + 1, dlugosc2(T, Next, Res).

% suma
suma(L, S) :- suma(L, 0, S).
suma([], Acc, Acc).
suma([H|T], Acc, Res) :- Tmp is Acc + H, suma(T, Tmp, Res).

% fib -- DZIAŁA
fib(K, N) :- fib(0, 0, 1, K, N).

fib(K, N, _, K, N).
fib(Index, A, B, K, N) :-
    (integer(N) -> N > A ; true),
    (integer(K) -> Index < K ; true),
        %    ( var(K)
        %    ->  ( var(N) 
        %        -> true
        %        ; N > A
        %        )
        %    ;   Index < K
        %    ),
    NextB is A + B,
    NextIndex is Index + 1,
    fib(NextIndex, B, NextB, K, N).

% poziom(ListaList, Poziom, ListaPoziomu)
poziom(LL, P, LP) :- poziom(LL, P, RLP, []), reverse(RLP, LP).

% poziom(ListaList, Poziom, OdwroconaListaPoziomu, Akumulator)
poziom([], _, Res, Res).
poziom(_, 0, Res, Res).
poziom([H|T], Lev, Res, Acc) :- 
    number(H),
    ( Lev > 1
    -> poziom(T, Lev, Res, Acc)
    ; poziom(T, 1, Res, [H|Acc])
    ).
poziom([[]|T], Lev, Res, Acc) :-
    poziom(T, Lev, Res, Acc).
poziom([[H|T]|TT], Lev, Res, Acc) :-
    Lev >= 1,
    poziom(TT, Lev, Res, Tmp),
    NewLev is Lev - 1,
    poziom([H|T], NewLev, Tmp, Acc).

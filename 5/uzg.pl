% uzg
uzg(List) :- uzg(List, _Result).

uzg([], _Result).
uzg([Result|Tail], Result) :- uzg(Tail, Result).

% --------------------------------

max2(L) :- max2(L, [_A, _B]).

max2([], _Possible).
max2([Head|Tail], Possible) :- member(Head, Possible), max2(Tail, Possible).

max2p(L) :- max2p(L, _A, _B).
max2p([], _A, _B).
max2p([A|Tail], A, B) :- max2p(Tail, A, B).
max2p([B|Tail], A, B) :- max2p(Tail, A, B).

% rozneZmienne

rozneZmienne([]).
%rozneZmienne([X|X]) :- rozneZmienne(X).
rozneZmienne([X|Y]) :- unify_with_occurs_check(X, Y), rozneZmienne(X).

% nawiasy

nawiasy(0'(, 0')).
nawiasy(0'[, 0']).

wyrNawiasowe(Expr) :- wyrNawiasowe(Expr, []).

wyrNawiasowe([], []).

wyrNawiasowe([Open|Tail], Stack) :- 
    nawiasy(Open, Close),
    wyrNawiasowe(Tail, [Close|Stack]).

wyrNawiasowe([Close|Tail], [Close|Stack]) :-
    wyrNawiasowe(Tail, Stack).

% ---------------------------

dobre(0, []).
dobre(s(0), []).

zmienne(0, []).
zmienne(s(N), [_Head|Tail]) :- zmienne(N, Tail).


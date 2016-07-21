% tuzPrzed(X, Y, L) 
tuzPrzed(X, Y, AXYB) :- append(_A, [X,Y|_B], AXYB).

% przed(X, Y, L)
przed(X, Y, L) :- append(_A, [X|B], L), member(Y, B).

% odwrotna(L, R)
odwrotna(L, R) :- odwrotna(L, [], R).

% odwrotna(L, A, R) :- R == reverse L ++ A
odwrotna([], Acc, Acc).
odwrotna([H|T], Acc, Res) :- odwrotna(T, [H|Acc], Res).

%palindrom(L) :- odwrotna(L, L).

palindrom(L) :- palindrom(L, []).

palindrom(L, L).
palindrom([_|L], L).
palindrom([H|T], A) :- palindrom(T, [H|A]).

sufiksy([], [[]]).
sufiksy([H|T], [[H|T]|X]) :- sufiksy(T, X).

fp(L, F) :- fp(L, [], F).

%fp([], Bs, Cs, R) :- append(Bs, Cs, R).
%fp([b|Rest], Bs, Cs, R) :- fp(Rest, [b|Bs], Cs, R).
%fp([c|Rest], Bs, Cs, R) :- fp(Rest, Bs, [c|Cs], R).

fp([], C, C).
fp([b|Rest], C, [b|Res]) :- fp(Rest, C, Res).
fp([c|Rest], C, Res) :- fp(Rest, [c|C], Res).

slowo([a|S]) :- slowo([a|S], []).

slowo([a|S], Bs) :- slowo(S, [b|Bs]).
slowo(L, L).

slowoR([a|X], Y) :- slowoR(X, [b|Y]).
slowoR([a,b|L], L).

slowoRp([a|X], Y) :- slowoRp(X, [b|Y]).
slowoRp(L, L).

% listy

:- op(500, 'xfy', '::').

% lista(X) wtw X jest prologową listą
lista([]).
lista([_|T]) :- lista(T).

% moja_lista(X)
moja_lista(nil).
moja_lista(_::Y) :- moja_lista(Y).

% pierwszy(E, L)
pierwszy([X|_], X).

% ostatni(E, L)
ostatni([X], X).
ostatni([_|X], Y) :- ostatni(X, Y).

% element(E, L).
element(X, [X|_]).
element(X, [_|Y]) :- element(X, Y).

% konwersja(X, Y)
konwersja([], nil).
konwersja([H|T], H::X) :- konwersja(T, X).

% intersection(L1, L2)
%intersection([X|_], [X|_]).
%intersection([_|X], Y) :- intersection(X, Y).
%intersection(X, [_|Y]) :- intersection(X, Y).
intersection(X, Y) :- member(Z, X), member(Z, Y).

% scal(L1, L2, L3) <=> L1 ++ L2 = L3
scal([], X, X).
scal([X|Y], Z, [X|U]) :- scal(Y, Z, U).

% podziel(L, Nieparz, Parz).
podziel([], [], []).
podziel([Head|Tail], Nieparz, [Head|Parz]) :- podziel(Tail, Parz, Nieparz).

% wypisz(L)
wypisz([]) :- write('Lista pusta.'), nl.
wypisz([X]) :- write(X), write('.'), nl.
wypisz([X, Y|Z]) :- write(X), write(', '), wypisz([Y|Z]).

% podlista(P, L).
podlista([], _).
podlista([B|C], ABCD) :- append(_A, BCD, ABCD), append([B|C], _D, BCD).

% podciag(P, L)
podciag([], _).
podciag([X|Y], [X|Z]) :- podciag(Y, Z).
podciag([X|Y], [_|Z]) :- podciag([X|Y], Z).

% srodek(E, L)
srodek(E, L) :- srodek(E, L, L).

srodek(E, [_], [E|_]).
srodek(E, [_, _|T], [_|U]) :- srodek(E, T, U).

rownej_dlugosci([], []).
rownej_dlugosci([_|X], [_|Y]) :- rownej_dlugosci(X, Y).

srodek2(E, L) :- append(X, [E|Y], L), rownej_dlugosci(X, Y).

% delete
delete(X, [X|Y], Y).
delete(X, [H|Y], [H|Z]) :- delete(X, Y, Z).

% perm
perm([], []).
perm([X|Y], [U|V]) :- delete(X, [U|V], T), perm(Y, T).

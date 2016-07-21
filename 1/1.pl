% kod
dziecko(jasio, ewa, jan).
dziecko(michal, ewa, jan).
dziecko(stasio, michal, barbara).
dziecko(franek, michal, barbara).

mezczyzna(jan).
mezczyzna(jasio).
mezczyzna(michal).
mezczyzna(siasio).
mezczyzna(franek).

rodzic(X, Y) :- dziecko(Y, _, X).
rodzic(X, Y) :- dziecko(Y, X, _).

ojciec(X, Y) :- dziecko(Y, _, X).

matka(X, Y) :- dziecko(Y, X, _).

dziecko(X, Y) :- dziecko(X, Y, _).
dziecko(X, Y) :- dziecko(X, _, Y).

syn(X, Y) :- mezczyzna(X), dziecko(X, Y).
corka(X, Y) :- kobieta(X), dziecko(X, Y).

kobieta(X) :- mezczyzna(X), !, fail.
kobieta(_).

przodek(X, Y) :- rodzic(X, Y).
przodek(X, Y) :- rodzic(Z, Y), przodek(X, Z).
%przodek(X, Y) :- przodek(X, Z), rodzic(Z, Y).

rodzenstwo(X, Y) :- dziecko(X, P, Q), dziecko(Y, P, Q).

spokrewniony(X, Y) :- przodek(Z, X), przodek(Z, Y).

babcia(X, Y) :- matka(X, Z), rodzic(Z, Y).

wnuczeto(X, Y) :- dziecko(X, Z), dziecko(Z, Y).



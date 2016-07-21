% even
even(0).
even(s(s(X))) :- even(X).

% odd
odd(s(0)).
odd(s(s(X))) :- odd(X).

% even2
even2(X) :- \+ odd(X).
odd2(X) :- \+ even(X).

% even3, odd3
even3(0).
even3(s(X)) :- odd3(X).
odd3(s(X)) :- even3(X).

% even4, odd4
even4(0).
even4(s(X)) :- \+ even4(X). % niewarstwowy, ale lokalnie warstwowy

odd4(s(X)) :- \+ odd4(X). % tak, to nie ma klauzuli unarnej!

% wf
wf(0, 1).
wf(1, 2).
wf(2, 0).


fna([]).
fna([H|T]) :- in_image(H), fna(T). 


in_image(Y) :- wf(_X, Y).
not_surj(D) :- member(X, D), \+ in_image(X).
fna2(D) :- \+ not_surj(D).

not_surj :- wf(X, _), \+ in_image(X).
fna :- \+ not_surj.


% pierwiatsek (z kolokwium)

pierw(N, K) :- pierw(N, K, 0, 0).

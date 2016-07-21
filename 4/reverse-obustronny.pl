% reverse

reverse(X, Y) :- reverse(X, Y, [], Y).

reverse([], X, X, []).
reverse([H|T], X, Acc, [_|Rest]) :- reverse(T, X, [H|Acc], Rest).

% delete
delete(X, [X|T], T, _).
delete(X, [H|T], [H|R], [_|Rest]) :- delete(X, T, R, Rest).

perm([], []).
perm([H1|T1], [H2|T2]) :- delete(H1, [H2|T2], X, [H1|T1]), perm(T1, X).

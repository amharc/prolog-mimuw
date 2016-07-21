% prolog
?- op(500, 'xfx', '--').

% poziomy
poziomy(D, LP) :- poziomy(D, [], LP).

poziomy(nil, L, L).
poziomy(tree(L, W, R), [], Res) :-
    poziomy(tree(L, W, R), [[]], Res).
poziomy(tree(L, WH -- WT, R), [WT | Rest], [WH | NewRest]) :-
    poziomy(R, Rest, RestTmp),
    poziomy(L, RestTmp, NewRest).

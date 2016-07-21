% flattemn
flatten(List, Res) :- flatten(List, [], Res).

flatten([], Acc, Acc).
flatten([H|T], Acc, Res) :-
    flatten(H, Tmp, Res),
    flatten(T, Acc, Tmp).
flatten(N, Acc, [N|Acc]) :-
    number(N).

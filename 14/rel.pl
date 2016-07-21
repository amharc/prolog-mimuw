% rel
rel(a, b).
rel(a, c).
rel(b, d).
rel(b, z).

% relSet
relSet(X, Z) :-
    relSet(X, [], Z).

relSet(X, Input, Result) :-
    rel(X, Y),
    nonmember(Y, Input),
    !,
    relSet(X, [Y | Input], Result).

relSet(_, Result, Result).

% relSet2
relSet2(X, Z) :-
   abolish(relSet2_mem/1),
   assert(relSet2_mem([])),
   relSet2(X),
   relSet2_mem(Z),
   abolish(relSet2_mem/1).

relSet2(X) :-
    rel(X, Y),
    relSet2_mem(Res),
    abolish(relSet2_mem/1),
    assert(relSet2_mem([Y | Res])),
    fail.

relSet2(_).

% set
set(List, Set) :-
    set(List, [], Set).

set([], Result, Result).
set([H|T], Input, Result) :-
    (member(H, Input)
        -> set(T, Input, Result)
        ;  set(T, [H|Input], Result)
    ).

% set2
set2(List, Set) :-
    set2_aux(List, Set),
    close_list(Set).

set2_aux([], _Set).
set2_aux([H|T], Set) :-
    member(H, Set),
    !,
    set2_aux(T, Set).

close_list(X) :- var(X), !, X = [].
close_list([]).
close_list([_|T]) :- close_list(T).

% set3
set3([], []).
set3([H|T], S) :-
    member(H, T),
    !,
    set3(T, S).
set3([H|T], [H|S]) :-
    set3(T, S).

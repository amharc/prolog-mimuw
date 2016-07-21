% last

last([H|T], E) :- last(T, H, E).
last([], H, H).
last([H|T], _, E) :- last(T, H, E).

% qsort(L, S)

qsort([], []).
qsort([Pivot|Rest], Result) :- 
    partition(Pivot, Rest, Less, GreaterEqual),
    qsort(Less, LessSorted),
    qsort(GreaterEqual, GreaterEqualSorted),
    append(LessSorted, [Pivot|GreaterEqualSorted], Result).

% partition(Element, List, Less, GreaterEqual)
partition([], _Pivot, [], []).

partition([Head|Tail], Pivot, [Head|LessTail], GreaterEqualTail) :-
    Pivot > Head,
    partition(Tail, Pivot, LessTail, GreaterEqualTail).

partition([Head|Tail], Pivot, LessTail, [Head|GreaterEqualTail]) :-
    Pivot =< Head,
    partition(Tail, Pivot, LessTail, GreaterEqualTail).

% qsort2(L, S)

qsort2(List, Result) :- qsort2(List, [], Result).

% qsort2(List, Suffix, Result) <=> Result = sorted(List) ++ Suffix
qsort2([], Suffix, Suffix).
qsort2([Pivot|Rest], Suffix, Result) :-
    partition(Pivot, Rest, Less, GreaterEqual),
    qsort2(GreaterEqual, Suffix, GreaterEqualSuffixSorted),
    qsort2(Less, [Pivot|GreaterEqualSuffixSorted], Result).

% flatten(LL, L).
flatten([], []).
flatten([Head|Tail], Result) :-
    flatten(Head, FlatHead),
    flatten(Tail, FlatTail),
    append(FlatHead, FlatTail, Result).
flatten([Num|Tail], [Num|FlatTail]) :-
    integer(Num),
    flatten(Tail, FlatTail).

% flatten2
flatten2(List, Result) :- flatten2(List, [], Result).

flatten2([], Suffix, Suffix).
flatten2(Num, Suffix, [Num|Suffix]) :- integer(Num).
flatten2([Head|Tail], Suffix, Result) :-
    flatten2(Tail, Suffix, FlatTail),
    flatten2(Head, FlatTail, Result).
%flatten2([Num|Tail], Suffix, [Num|FlatTail]) :-
%    integer(Num), 
%    flatten2(Tail, Suffix, FlatTail).

% qsort3
qsort3(List, Result) :- qsort3(List, [], Result).

% qsort3(List, Suffix, Result) <=> Result = sorted(List) ++ Suffix
qsort3([], Suffix, Suffix).
qsort3([Pivot|Rest], Suffix, Result) :-
    partition3(Rest, Pivot, Less, GreaterEqual, Diff),
    ( Diff @> 0
        ->
            qsort3(GreaterEqual, Suffix, GreaterEqualSuffixSorted),
            qsort3(Less, [Pivot|GreaterEqualSuffixSorted], Result)
        ;
            qsort3(Less, [Pivot|GreaterEqualSuffixSorted], Result),
            qsort3(GreaterEqual, Suffix, GreaterEqualSuffixSorted)
    ).

partition3([], _Pivot, [], [], 0).

partition3([Head|Tail], Pivot, [Head|LessTail], GreaterEqualTail, NewDiff) :-
    Pivot > Head,
    partition3(Tail, Pivot, LessTail, GreaterEqualTail, Diff),
    NewDiff is Diff + 1.

partition3([Head|Tail], Pivot, LessTail, [Head|GreaterEqualTail], NewDiff) :-
    Pivot =< Head,
    partition3(Tail, Pivot, LessTail, GreaterEqualTail, Diff),
    NewDiff is Diff - 1.

longer([], [_|_]).
longer([_|X], [_|Y]) :- longer(X, Y).


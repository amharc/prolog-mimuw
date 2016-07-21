:- op(500, 'xfx', '--').

% flagaPolska bez akumulatora
flagaPolska([], List -- List).
flagaPolska([a | T], [a | Front] -- Back) :-
    flagaPolska(T, Front -- Back).
flagaPolska([b | T], Front -- Back) :-
    flagaPolska(T, Front -- [b | Back]).

flagaPolska2(List, Res) :- flagaPolska(List, Res -- []).

% flagaPolska z akumulatorem
flagaPolskaAcc(List, Res) :-
    flagaPolskaAcc(List, Res, Empty -- Empty).

flagaPolskaAcc([], Res, Res -- []).
flagaPolskaAcc([a | T], [a | Res], Front -- Back) :-
    flagaPolskaAcc(T, Res, Front -- Back).
flagaPolskaAcc([b | T], Res, Front -- Back) :-
    flagaPolskaAcc(T, Res, [b | Front] -- Back).

% DFS
%dfs(nil, Res -- Res).
%dfs(tree(Left, Elem, Right), Front -- Back) :-
%    dfs(Left, Front -- [Elem | Middle]),
%    dfs(Right, Middle -- Back).

% wszerz
wszerz(Tree, Result) :-
    wszerz_aux([Tree | Empty] -- Empty, Result).

% UWAGA: Unfiikacja Result do [] dopiero po odcięciu, bo inaczej NIE DZIAŁA dla ustalonych
wszerz_aux(Front -- Back, Result) :- Front == Back, !, Result = [].
wszerz_aux([nil | Front] -- Back, Result) :-
    wszerz_aux(Front -- Back, Result).
wszerz_aux([tree(Left, Elem, Right) | Front] -- [Left, Right | Back], [Elem | Result]) :-
    wszerz_aux(Front -- Back, Result).



% flagaHolenderska
flagaHolenderska(List, Res) :-
    flagaHolenderska(List, Res, Empty -- Empty).

flagaHolenderska([], Res, Res -- []).
flagaHolenderska([a | T], [a | Res], Front -- Back) :-
    flagaHolenderska(T, Res, Front -- Back).
flagaHolenderska([b | T], Res, Front -- Back) :-
    flagaHolenderska(T, Res, [b | Front] -- Back).
flagaHolenderska([c | T], Res, Front -- [c | Back]) :-
    flagaHolenderska(T, Res, Front -- Back).

% flagaHolenderska z trzema akumulatorami
flagaHolenderska3(List, Res) :-
    flagaHolenderska3(List, Res -- ABack, ABack -- BBack, BBack -- []).

flagaHolenderska3([], EmptyA -- EmptyA, EmptyB -- EmptyB, EmptyC -- EmptyC).
flagaHolenderska3([a | T], [a | FrontA] -- BackA, B, C) :-
    flagaHolenderska3(T, FrontA -- BackA, B, C).
flagaHolenderska3([b | T], A, [b | FrontB] -- BackB, C) :-
    flagaHolenderska3(T, A, FrontB -- BackB, C).
flagaHolenderska3([c | T], A, B, [c | FrontC] -- BackC) :-
    flagaHolenderska3(T, A, B, FrontC -- BackC).

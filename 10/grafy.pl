% connect
connectT(A, B, Graph) :-
    member(edge(A, V), Graph),
    reachableT(V, B, Graph).

reachableT(A, A, _Graph).
reachableT(A, B, Graph) :-
    member(edge(A, V), Graph),
    reachableT(V, B, Graph).

connect(A, B) :-
    edge(A, V),
    reachable(V, B).

reachable(A, A).
reachable(A, B) :-
    edge(A, V),
    reachable(V, B).

%-------------------------

connect2(A, B) :- edge(A, B).
connect2(A, B) :- edge(A, V), reachable(V, B).

% path

path(A, B, [A, B]) :- edge(A, B).
path(A, B, [A | Rest]) :- edge(A, V), path(V, B, Rest).

% pathC

pathC(A, B, Path) :-
    edge(V, B),
    pathC(A, V, [V], Path).

pathC(A, A, Path, Path).
pathC(A, B, Visited, Path) :-
    edge(V, B),
    \+ member(V, Visited),
    pathC(A, V, [V | Visited], Path).

% euler

euler(Graph, Path) :-
    vertex(V, Graph),
    euler(Graph, V, Path).

euler([], _V, []).
euler(Graph, V, [V|Path]) :-
    pick_edge(V, Graph, U, NewGraph),
    euler(NewGraph, U, Path).

pick_edge(V, [edge(V, U)|Rest], U, Rest).
pick_edge(V, [edge(U, V)|Rest], U, Rest).
pick_edge(V, [Head|Rest], U, [Head|NewRest]) :-
    pick_edge(V, Rest, U, NewRest).

vertex(V, [edge(V, _)|_]).
vertex(U, [edge(_, U)|_]).
vertex(V, [_|Rest]) :- vertex(V, Rest).

% NIEOK
%root(V, Graph) :-
%    member(edge(V, _U), Graph),
%    \+ member(edge(_U, V), Graph).

root(V, Graph) :-
    member(edge(V, _U), Graph),
    \+ enters(V, Graph).

enters(V, Graph) :- member(edge(_, V), Graph).

tree(Graph, Tree) :-
    root(V, Graph), !,
    tree(Graph, V, Tree, [V], [], _).

tree(Graph, V, nil, Visited, Graph, Visited) :-
    \+ member(edge(V, _), Graph).

tree(Graph, V, tree(Left, V, Right), Visited, NewGraph, NewVisited) :-
    pick_dir_edge(V, Graph, LV, LGraph1),
    \+ member(LV, Visited),

    tree(LGraph1, LV, Left, Visited, LGraph, LVisited),

    pick_dir_edge(V, LGraph, RV, RGraph1),
    \+ member(RV, Visited),

    tree(RGraph1, RV, Right, LVisited, NewGraph, NewVisited).

pick_dir_edge(V, [edge(V, U)|Rest], U, Rest).
pick_dir_edge(V, [Head|Rest], U, [Head|NewRest]) :-
    pick_dir_edge(V, Rest, U, NewRest).



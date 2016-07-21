% insertBST

%insertBST(Elem, Leaf) :-
%    var(Leaf),
%    !, % RED CUT
%    Leaf = tree(_Left, Elem, _Right).

% BEZ DUPLIKATÓW
insertBST(Elem, tree(_Left, Elem, _Right)) :- !.

insertBST(Elem, tree(Left, Node, Right)) :-
    ( Elem < Node
    -> insertBST(Elem, Left)
    ; insertBST(Elem, Right)
    ).

% closeD

closeD(Leaf) :-
    var(Leaf),
    !,
    Leaf = nil.

closeD(tree(Left, _Node, Right)) :-
    closeD(Left),
    closeD(Right).

% createBST

createBST([], Tree) :- closeD(Tree).
createBST([H | T], Tree) :-
    insertBST(H, Tree),
    createBST(T, Tree).

% infixBST
infixBST(Tree, List) :-
    infixBST(Tree, [], List).

infixBST(nil, Acc, Acc).
infixBST(tree(Left, Elem, Right), Acc, Result) :-
    infixBST(Right, Acc, Temp),
    infixBST(Left, [Elem | Temp], Result).

% sortBST
sortBST(List, Sorted) :-
    createBST(List, Tree),
    infixBST(Tree, Sorted).

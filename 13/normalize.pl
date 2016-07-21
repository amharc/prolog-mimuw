% wyrażenie różnicowe: expr(Expr, LastAtom, Cont)

%normalize
normalize(Expr, Normalised) :- normalize_d(Expr, expr(Normalised, LastAtom, LastAtom)).

normalize_d(Atom, expr(Expr, Atom, Expr)) :- atom(Atom).
normalize_d(A+B, expr(Expr, LastAtom, Cont)) :-
    normalize_d(A, expr(Expr, LastAtomA, LastAtomA + ExprB)),
    normalize_d(B, expr(ExprB, LastAtom, Cont)).

% -------------------------------------------------------------------


?- op(500, 'xfx', '++').

normalize2(Expr, Normalised) :- normalize2_d(Expr, Normalised ++ 0).
normalize2_d(Atom, Atom ++ 0) :- atom(Atom).
normalize2_d(Atom, (Atom + Cont) ++ Cont) :- atom(Atom), var(Cont).
normalize2_d(A + B, Expr ++ Cont) :-
    normalize2_d(A, Expr ++ ContA),
    normalize2_d(B, ContA ++ Cont).

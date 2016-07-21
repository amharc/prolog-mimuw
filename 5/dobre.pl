% dobre

dobre(0, []).
dobre(s(0), []).
dobre(s(s(0)), [para(X, Y), para(Y, X)]).
dobre(s(s(s(N))), [para(X, Y), para(Y, Z) | Tail]) :- dobre(s(s(N)), [para(X, Z) | Tail]).

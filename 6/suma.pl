% suma

% M1 jest unifikowane z P2
suma(liczba(P1, M1), liczba(M1, M2), liczba(P1, M2)).

suma([], liczba(X, X)).
suma([liczba(P1, M1)|T], liczba(P1, M2)) :- suma(T, liczba(M1, M2)).

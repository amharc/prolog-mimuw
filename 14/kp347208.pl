% Krzysztof Pszeniczny
% 347208

osiagalne(W, L) :-
    osiagalne([sciezka(W, 0, [W])], [odwiedzony(W, 0, [W])], W, L).

% osiagalne(DoPrzeszukania, Odwiedzone, Zrodlo, Wynik)
%  przechodzi po grafie, w DoPrzeszukania ma "kolejke" wierzchołków (wraz z
%  parzystościami ściezki od korzenia i samą tą ścieżką), z których należy
%  jeszcze rozpocząc przeszukiwanie, w Odwiedzone jest lista odwiedzonych już
%  par odwiedzony(wierzchołek, parzystosc, sciezka),
%  Zrodlo to korzen drzewa przeszukiwań
%
%  Trzymanie pełnych ścieżek jest istotne, np. z powodu grafu:
%    graf([a, b, c]).
%    sasiedzi(a, [kr(b, 0), kr(c, 0)]).
%    sasiedzi(b, [kr(c, 0)]).
%    sasiedzi(c, [kr(b, 1)]).
%  Wtedy jedyną drogą do b nieparzystą jest a --> c --> b, jednakże jeślibyśmy
%  zapisywali jedynie fakt odwiedzenia wierzchołka c z wagą 0, to jeśli algorytm
%  najpierw rozpatrzyłby a --> b --> c -X-> b (ostatniej krawędzi nie rozpatrzy,
%  bo cykl) i zapamiętał, że nie ma potrzeby odwiedzać (c, 0) ponownie, to 
%  nie uzyskalibyśmy poprawnej ściezki a --> c --> b.
osiagalne([], Visited, _Zrodlo, List) :-
    nieparzyste(Visited, [], List).
osiagalne([sciezka(W, Parz, Sciezka)|T], Visited, Zrodlo, List) :-
    sasiedzi(W, Krawedzie),
    dodaj_wszystkie(Krawedzie, W, Parz, Sciezka, T, Visited, Zrodlo, TT, VV),
    osiagalne(TT, VV, Zrodlo, List).

% nieparzyste(Visited, Akumulator, Wynik)
%   filtruje te odwiedzone wierzchołki, do których istniała ścieżka o
%   nieparzystej wadze
nieparzyste([], Akumulator, Akumulator).
nieparzyste([odwiedzony(_W, 0, _Sciezka)|T], Acc, Res) :-
    nieparzyste(T, Acc, Res).
nieparzyste([odwiedzony(W, 1, _Sciezka)|T], Acc, Res) :-
    ( member(W, Acc)
    -> nieparzyste(T, Acc, Res)
    ;  nieparzyste(T, [W | Acc], Res)
    ).

% dodaj_wszystkie(Krawedzie, Wierzcholek, Sciezka, DoPrzeszukania, Visited,
%                 Zrodlo, NoweDoPrzeszukania, NoweVisited)
%  wykonuje jeden krok "pseudo-BFS"-a: dla każdego kr(V, Parzystosc) rozpatruje
%  krawedz z Wierzchołek do V o parzystosci Parzystośc.
%
%  Sprawdza, czy nie tworzą się cykle (używając przekazanej ścieżki od korzenia
%  (Zrodlo) i rozpatruje osobno przypadek utworzenia cyklu do korzenia.
dodaj_wszystkie([], _W, _Parz, _Sciezka, T, V, _Zrodlo, T, V).
dodaj_wszystkie([kr(V, ParzKr)|Krawedzie], W, Parz,
                Sciezka, T, Vis, Zrodlo, TT, VV) :-
    przejdz(Parz, ParzKr, NowaParz),
    ( member(odwiedzony(V, NowaParz, Sciezka), Vis)
    -> NewT = T, NewVis = Vis % zignoruj krawędź, już była rozpatrywana
    ; ( V = Zrodlo % cykl do źródła
        -> NewT = T, NewVis = [odwiedzony(V, NowaParz, Sciezka)|Vis]
        ; ( member(V, Sciezka)
            -> NewT = T, NewVis = Vis % ścieżka zawierałaby cykl
            ;  NewT = [sciezka(V, NowaParz, [V|Sciezka])|T],
               NewVis = [odwiedzony(V, NowaParz, Sciezka)|Vis]
          )
      )
    ),
    dodaj_wszystkie(Krawedzie, W, Parz, Sciezka, NewT, NewVis, Zrodlo, TT, VV).

% Dodawanie modulo 2.
przejdz(0, 0, 0).
przejdz(0, 1, 1).
przejdz(1, 0, 1).
przejdz(1, 1, 0).

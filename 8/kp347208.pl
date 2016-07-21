% Krzysztof Pszeniczny
% 347208

sumy(Lista, ListaSum) :-
    sumy(Lista, [], ListaSum).


% sumy(Lista, Akumulator, ListaSum) wtedy i tylko wtedy, gdy 
%   ListaSum[i] = Akumulator[i] + (suma elementów Lista na poziomie i)
%   przy czym Akumulator jest w razie potrzeby uzupełniany zerami
sumy(Whatever, [], Res) :-
    !, % zielone odcięcie, żeby nie tworzyć punktu nawrotu
       % (jeśli drugi arg. to [] to nie można użyć żadnej innej klauzuli)
    sumy(Whatever, [0], Res). % rozszerzenie akumulatora

sumy([], [AccHead|AccTail], [AccHead|AccTail]).

sumy([Head|Tail], [AccHead|AccTail], Res) :-
    ( number(Head) ->
        NewAccHead is AccHead + Head,
        sumy(Tail, [NewAccHead|AccTail], Res)
    ;   sumy(Head, AccTail, NewAccTail),
        sumy(Tail, [AccHead|NewAccTail], Res)
    ).

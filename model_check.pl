% Krzysztof Pszeniczny, 347208

?- ensure_loaded(library(lists)).

?- op(500, 'xfx', '--').
?- op(700, 'xfx', '<>').

% Arrays are represented as lists of pairs
%   Index--Value
%
% State is represented by state(vars, arrays, counters),
% where:
%   vars -- a list of variables (name--value pairs)
%   arrays -- a list of arrays (name--array pairs)
%             with each array being represented as a list (index--value pairs)
%   counters -- a list of program counters (pid--counter pairs)
%
% This way, every array is represented by an associative list.
% This is slightly more general than in the problem statement,
% because I explicitly allow arbitrary array indices (as long
% as the model remains finite). Of course, this incurs a slightly
% bigger memory usage, but we are supposed not to worry too much
% about performance in this task.

% readArrayZero(+Array, +Index, -Value) is det.
%
% readArrayZero(+Array, +Index, -Value) <=> Array[Index] = Value
% If the value is not present, 0 is returned.
readArrayZero(Array, Index, Value) :-
    ( member(Index--Value, Array)
      -> true
      ;  Value = 0
    ).

% readArrayEmpty(+Array, +Index, -Value) is det.
%
% readArrayEmpty(+Array, +Index, -Value) <=> Array[Index] = Value
% If the value is not present, 0 is returned.
readArrayEmpty(Array, Index, Value) :-
    ( member(Index--Value, Array)
      -> true
      ;  Value = []
    ).

%% writeArray(+Array, +Index, +Value, -NewArray) is det.
%
% writeArray(+Array, +Index, +Value, -NewArray) <=>
%   NewArray[J] = Array[J] for J /= Index, = Value for J = Index
writeArray([], Index, Value, [Index--Value]).
writeArray([Index--_Value|Rest], Index, Value, [Index--Value|Rest]) :-
    !. % green cut -- indices/keys are unique
writeArray([AIndex--AValue|Rest], Index, Value, [AIndex--AValue|NewRest]) :-
    AIndex \= Index,
    writeArray(Rest, Index, Value, NewRest).

%% evalExpr(+Expr, +State, +Pid, -Result) is det.
%
% evalExpr(Expr, State, Pid, Result) <=> Expr evaluates to Result in state State
evalExpr(N, _State, _Pid, N) :-
    number(N),
    !.
evalExpr(pid, _State, Pid, Pid) :-
    !.
evalExpr(Var, state(Vars, _Arrays, _Counters), _Pid, N) :-
    atom(Var),
    Var \= pid,
    !,
    readArrayZero(Vars, Var, N).
evalExpr(arr(ArrayName, IndexExpr), State, Pid, N) :-
    !,
    State = state(_Vars, Arrays, _Counters),
    evalExpr(IndexExpr, State, Pid, Index),
    readArrayEmpty(Arrays, ArrayName, Array),
    readArrayZero(Array, Index, N).
evalExpr(Term, State, Pid, N) :-
    Term =.. [Oper, LhsExpr, RhsExpr],
    member(Oper, [+, -, *, /]),
    !,
    evalExpr(LhsExpr, State, Pid, Lhs),
    evalExpr(RhsExpr, State, Pid, Rhs),
    Result =.. [Oper, Lhs, Rhs],
    N is Result.

%% translateBoolOp(+Given, -Result).
%
% Performs a translation of operators of the input language to prolog ones.
translateBoolOp(<>, =\=).
translateBoolOp(=, =:=).
translateBoolOp(<, <).

%% evalBoolExpr(+State, +State, +Pid) is semidet.
%
% evalBoolExpr(Expr, State, Pid) <=> Expr evaluates to true in state State
evalBoolExpr(Term, State, Pid) :-
    Term =.. [Oper, LhsExpr, RhsExpr],
    translateBoolOp(Oper, NewOper),
    !,
    evalExpr(LhsExpr, State, Pid, Lhs),
    evalExpr(RhsExpr, State, Pid, Rhs),
    call(NewOper, Lhs, Rhs).

%% incrementCounter(+State, +Pid, -NewState) is det.
% 
% increments the program counter of the given process
incrementCounter(state(Vars, Arrays, Counters), Pid, 
        state(Vars, Arrays, NewCounters)) :-
    readArrayZero(Counters, Pid, Counter),
    NewCounter is Counter + 1,
    writeArray(Counters, Pid, NewCounter, NewCounters).

%% assignVar(+State, +Var, +Value, -NewState) is det.
%
% assigns a new value of a variable
assignVar(state(Vars, Arrays, Counters), Var, Value,
        state(NewVars, Arrays, Counters)) :-
    writeArray(Vars, Var, Value, NewVars).

%% assignArr(+State, +ArrayName, +Index, +Value, -NewState) is det.
%
% assigns a new value of an array element
assignArr(state(Vars, Arrays, Counters), ArrName, Index, Value,
        state(Vars, NewArrays, Counters)) :-
    readArrayEmpty(Arrays, ArrName, Arr),
    writeArray(Arr, Index, Value, NewArr),
    writeArray(Arrays, ArrName, NewArr, NewArrays).

%% assignStmt(+Stmt, +State, +Pid, -NewState) is det.
%
% executes a single statement
execStmt(assign(Var, Expr), State, Pid, NewState) :-
    atom(Var),
    evalExpr(Expr, State, Pid, Value),
    assignVar(State, Var, Value, TmpState),
    incrementCounter(TmpState, Pid, NewState).
execStmt(assign(arr(ArrName, IndexExpr), Expr), State, Pid, NewState) :-
    evalExpr(Expr, State, Pid, Value),
    evalExpr(IndexExpr, State, Pid, Index),
    assignArr(State, ArrName, Index, Value, TmpState),
    incrementCounter(TmpState, Pid, NewState).
execStmt(goto(NewCounter), state(Vars, Arrays, Counters), Pid,
        state(Vars, Arrays, NewCounters)) :-
    writeArray(Counters, Pid, NewCounter, NewCounters).
execStmt(condGoto(Expr, NewCounter), State, Pid, NewState) :-
    ( evalBoolExpr(Expr, State, Pid)
    -> execStmt(goto(NewCounter), State, Pid, NewState)
    ; incrementCounter(State, Pid, NewState)
    ).
execStmt(sekcja, State, Pid, NewState) :-
    incrementCounter(State, Pid, NewState).

%% step(+Program, +State, +Pid, -NewState) is det.
%% step(+Program, +State, ?Pid, -NewState) is nondet.
%
% Performs a single step of the simulation.
step(Program, State, Pid, NewState) :-
    State = state(_Vars, _Arrays, Counters),
    member(Pid--Counter, Counters),
    nth1(Counter, Program, Stmt),
    execStmt(Stmt, State, Pid, NewState).

%% fillArray(+N, ?Value, ?Array) is det.
%
% fillArray(N, Value, Array) <=> Array = [i--Value | i <- [0..N-1]].
fillArray(N, Value, Counters) :-
    ( N = 0
        -> Counters = []
        ; ( NewN is N - 1,
            Counters = [NewN -- Value | RestCounters],
            fillArray(NewN, Value, RestCounters)
          )
    ).

%% initState(+ProgramDesc, +NumProcs, -State) is det.
%
% build the initial state
initState(program(VarNames, ArrayNames, _Program), NumProcs,
        state(Vars, Arrays, Counters)) :-
    fillArray(NumProcs, 1, Counters),
    fillArray(NumProcs, 0, DefaultArray),
    fill(VarNames, 0, Vars),
    fill(ArrayNames, DefaultArray, Arrays).

%% fill(+Names, +Value, ?List) is det.
%
% fill(Names, Val, List) <=> List = [Name--Val | Name <- Names]
fill([], _Value, []).
fill([VarName|TailNames], Value, [VarName--Value|Tail]) :-
    fill(TailNames, Value, Tail).

readProgram(Stream, program(Vars, Arrays, Program)) :-
    read(Stream, vars(Vars)),
    read(Stream, arrays(Arrays)),
    read(Stream, program(Program)).

readProgramFile(FileName, Program) :-
    catch(
        open(FileName, read, Stream),
        error(existence_error(source_sink, Path), _),
        (format('Error: brak pliku o nazwie - ~w~n', [Path]),
         fail)
    ),
    readProgram(Stream, Program),
    close(Stream).

%% inCritSection(+Counters, +Program, -InSection) is det.
%
% InSection = list of processes currently executing the critical section
inCritSection([], _Program, []).
inCritSection([Pid--Counter|RestCounters], Program, InSection) :-
    ( nth1(Counter, Program, sekcja)
        -> InSection = [Pid | Rest],
           inCritSection(RestCounters, Program, Rest)
        ;  inCritSection(RestCounters, Program, InSection)
    ).

%% allMovesFrom(+State, +Program, +Visited, +Path, -Result) is det.
%
% Performs a depth-first search starting from State.
% Visited - list of states visited previously.
% Path - list of (pid -- counter) pairs representing moves
%        which led to the current state being visited
%
% Then either:
%  - Result = safe(NewVisited) iff the depth-first search found
%       that no unsafe state is reachable from State
%  - Result = counterexample(InCritSection, UnsafeStateId, Trail)
%      when an unsafe state (identified by its id: UnsafeStateId)
%      is reachable from State. InCritSection is the list of processes
%      being in the critical section in this state, Trail is the trail
%      (list of pairs pid -- counter) leading from the initial state to the
%      found unsafe one.
%
% Depth-first search is chosen, as it is the default in `real' naive
% model-checkers (e.g. SPIN). Visited states are memoised for efficiency,
% so this predicate unfortunately is deterministic and does not leverage
% Prolog's built-in nondeterminism.
allMovesFrom(State, Program, Visited, Path, Result) :-
    State = state(_Vars, _Arrays, Counters),
    ( inCritSection(Counters, Program, [Cr0, Cr1 | CrRest])
    -> length(Visited, LenVisited),
       Result = counterexample([Cr0, Cr1 | CrRest], LenVisited, Path)
    ; allMovesFrom(Counters, State, Program, Visited, Path, Result)
    ).

%% allMovesFrom(+Counters, +State, +Program, +Visited, +Path, -Result) is det.
%
% A worker predicate for allMovesFrom/5.
% It tries to perform moves from state State as specified by the Counters
% provided -- i.e. tries to perform a step of any of the processes.
allMovesFrom([], _State, _Program, Visited, _Path, safe(Visited)).
allMovesFrom([Pid--Counter|Counters], State, Program, Visited, Path, Result) :-
    step(Program, State, Pid, NewState),
    ( member(NewState, Visited)
    -> Tmp = safe(Visited)
    ; allMovesFrom(NewState, Program, [NewState | Visited],
            [Pid--Counter|Path], Tmp)
    ),
    extend(Tmp, Counters, State, Program, Path, Result).

%% extend(+Result, +Counters, +State, +Program, +Path, -Result) is det.
%
% A worker predicate for allMovesFrom/6.
% It processes the result obtained from the nested invocation of allMovesFrom/5.
% If the nested call found a counterexample it is returned immediately.
% On the other hand, if no unsafe state was found, it retrieves the new Visited
% states set and continues with the execution of allMovesFrom/6.
%
% It serves as some kind of monadic bind, threading `exceptions' (i.e. found
% trails) and `state' (i.e. the visited states set).
extend(counterexample(InCrit, LenVisited, Path), _Counters, _State, _Program,
        _Path, counterexample(InCrit, LenVisited, Path)).
extend(safe(Visited), Counters, State, Program, Path, Result) :-
    allMovesFrom(Counters, State, Program, Visited, Path, Result).

verify(NumProcs, FileName) :-
    (   checkNumProcs(NumProcs),
        readProgramFile(FileName, Program)
    ->  initState(Program, NumProcs, InitState),
        Program = program(_VarNames, _ArrayNames, Statements),
        (allMovesFrom(InitState, Statements, [InitState], [],
                    counterexample(InCrit, LenVisited, Counterexample))
            -> showCounterexample(InCrit, LenVisited, Counterexample)
            ;  format('Program jest poprawny (bezpieczny).~n', [])
        )
    ;   true % verify should always succeed
    ).

checkNumProcs(N) :-
    integer(N),
    N > 0.
checkNumProcs(N) :-
    integer(N),
    N =< 0,
    format('Error: parametr 0 powinien byc liczba > 0~n', []),
    fail.
checkNumProcs(N) :-
    \+ integer(N),
    format('Error: parametr 0 powinien byc liczba~n', []),
    fail.

showCounterexample(InCrit, LenVisited, Counterexample) :-
    format('Program jest niepoprawny: stan nr ~d nie jest bezpieczny.~n', 
        [LenVisited]),
    format('Niepoprawny przeplot:~n', []),
    reverse(Counterexample, Trail),
    showTrail(Trail),
    write('Procesy w sekcji: '),
    showInSection(InCrit).

showInSection([Proc]) :- 
    !, % green cut
    format('~d.~n', [Proc]).
showInSection([Proc, Proc2|Procs]) :-
    format('~d, ', [Proc]),
    showInSection([Proc2|Procs]).

showTrail([]).
showTrail([Pid--Counter|Tail]) :-
    format('    Proces ~d:   ~d~n', [Pid, Counter]),
    showTrail(Tail).

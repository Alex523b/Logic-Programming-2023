% Αλέξανδρος Ντιβέρης - 1115201900136

:- lib(ic).
:- lib(ic_global).
:- compile(skyscr_data).

skyscr(PuzzleID, Solution) :-
    puzzle(PuzzleID, N, L1, L2, L3, L4, Solution),
    constrain_rows(Solution, N),
    constrain_columns(Solution, N),
    constrain_view_(
    search(Solution, 0, first_fail, indomain, complete, []),
    Solution = Solution.

constrain_rows([], _).
constrain_rows([H|L], N) :-
    H #:: 1..N,
    ic:alldifferent(H),
    constrain_rows(L, N).
constrain_columns(Solution, N) :-
    transpose(Solution, 1, N, TransposedMatrix),
    constrain_rows(TransposedMatrix, N).

transpose(_, I, M, []) :-
    I > M.
transpose(Matrix, I, M, [Column|T]) :-
    UpdatedI is I + 1,
    nth_column(Matrix, I, Column),
    transpose(Matrix, UpdatedI, M, T).

nth_row([H|_], 1, H) :- !.
nth_row([_|T], I, X) :-
    I1 is I - 1,
    nth_row(T, I1, X).

nth_column([], _, []).
nth_column([H|T], I, [R|X]) :-
    nth_row(H, I, R), 
    nth_column(T, I, X).
    
test(N, V, F) :-
    length(F, N),
    F #:: 1..N,
    ic_global:alldifferent(F),
    maximize(F, [], NewF),
    nvalue(V, NewF),
    append(F, NewF, Test),
    labeling(Test).

maximize([], _, []).
maximize([H|L], SoFarHList, [MaxH|T]) :-
    append(SoFarHList, [H], NewSoFarHList),
    MaxH #= max(NewSoFarHList),
    maximize(L, NewSoFarHList, T).
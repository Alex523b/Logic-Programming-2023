% Αλέξανδρος Ντιβέρης - 1115201900136

:- lib(ic).
:- lib(ic_global).
:- compile(skyscr_data).

skyscr(PuzzleID, Solution) :-
    puzzle(PuzzleID, N, L1, L2, L3, L4, Solution),
    Solution #:: 1..N,

    constrain_rows(Solution),
    constrain_row_views(Solution, L1, L2),

    constrain_columns(Solution, N, L3, L4),

    search(Solution, 0, max_regret, indomain_middle, complete, []), !.

constrain_rows([]).
constrain_rows([H|L]) :-
    ic:alldifferent(H), % skyscrapers should be of different size per row
    constrain_rows(L).

constrain_columns(Solution, N, L1, L2) :-
    transpose(Solution, 1, N, TransposedSolution),
    % columns of Solution are the rows of TransposedSolution; thus, we can handle them as rows
    constrain_rows(TransposedSolution),
    constrain_row_views(TransposedSolution, L1, L2).

constrain_row_views(Solution, L1, L2) :-
    constrain_front_row_view(Solution, L1),
    constrain_rear_row_view(Solution, L2).

constrain_front_row_view([], []).
constrain_front_row_view([H|T], [V|Rest]) :-
    (V =\= 0 ->
        maximize(H, [], MaxH),
        nvalue(V, MaxH) % if nvalue MaxH contains V different values, then V skyscrapers are visible from one side
        ;
        true
    ),
    constrain_front_row_view(T, Rest).

constrain_rear_row_view([], []).
constrain_rear_row_view([H|T], [V|Rest]) :-
    reverse(H, RevH), % to constrain the rear view of a row, simply constrain the front view of the reversed row version 
    constrain_front_row_view([RevH], [V]),
    constrain_rear_row_view(T, Rest).

maximize([], _, []).
maximize([H|L], HList, [MaxH|T]) :-
    MaxH #= max([H|HList]),
    maximize(L, [H|HList], T).

% the following predicates have also been used in crossword puzzle exercise
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

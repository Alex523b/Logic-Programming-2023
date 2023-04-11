% Αλέξανδρος Ντιβέρης - 1115201900136

:- lib(ic).

numpart(N, L1, L2) :-
    N mod 2 =:= 0,
    N_half is N // 2,
    length(L1, N_half),
    length(L2, N_half),
    L1 #:: 1..N,
    alldifferent(L1),
    alldifferent(L2),
    L2 #:: 1..N,
    append(L1, L2, L3),
    alldifferent(L3),
    sum(L1) #= sum(L2),
    compute_squares(L1, L1_squared),
    compute_squares(L2, L2_squared),
    sum(L1_squared) #= sum(L2_squared),

    is_sorted(L1),
    is_sorted(L2),
    append([X], _, L1),
    append([Y], _, L2),
    X #< Y,
    search([L1, L2], 0, first_fail, indomain, complete, []).


compute_squares([],[]).
compute_squares([H|L], [H * H|L1]) :-
    compute_squares(L, L1).

is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|L]) :-
    X #=< Y,
    is_sorted([Y|L]).
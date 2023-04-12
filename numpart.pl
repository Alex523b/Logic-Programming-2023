% Αλέξανδρος Ντιβέρης - 1115201900136

:- lib(fd).

numpart(N, L1, L2) :-
    variables(N, L1, L2, L3),
    constrain(L1, L2, L3),
    generate_most_constrained(L3).

variables(N, L1, L2, L3) :-
    length(L3, N),
    L3 :: 1..N,
    N_half is N // 2,
    length(L1, N_half),
    length(L2, N_half),
    append(L1, L2, L3).

constrain(L1, L2, L3) :-
    alldifferent(L3),
    sum(L1) #= sum(L2),

    compute_squares(L1, L1_squared),
    compute_squares(L2, L2_squared),
    sum(L1_squared) #= sum(L2_squared),

    % eliminate permutations in each list
    is_sorted(L1),
    is_sorted(L2),

    % eliminate permutation of L1 and L2
    append([X], _, L1),
    X #= 1.

generate_most_constrained([]).
generate_most_constrained(Columns) :-
   deleteffc(Column, Columns, RestColumns),
   indomain(Column),
   generate_most_constrained(RestColumns).

% auxiliary predicates
compute_squares([],[]).
compute_squares([H|L], [H * H|L1]) :-
    compute_squares(L, L1).

is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|L]) :-
    X #=< Y,
    is_sorted([Y|L]).

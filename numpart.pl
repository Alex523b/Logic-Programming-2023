% Αλέξανδρος Ντιβέρης - 1115201900136

:- lib(gfd).

numpart(N, L1, L2) :-
    N mod 2 =:= 0, % ensure that N is even
    variables(N, L1),

    % constraints
    alldifferent(L1),
    is_sorted(L1), % symmetry breaking constraint
    sum(L1) #= (N*(N+1)) / 4,

    compute_squares(L1, L1_squared),
    sum(L1_squared) #= (N*(N + 1)*(2*N + 1)) / 12,

    append([X], _, L1),
    X #= 1, % symmetry breaking constraint

    search(L1, 0, most_constrained, indomain, complete, []),

    all_between(1, N, L3),
    set_difference(L3, L1, L2).

variables(N, L1) :-
    N_half is N // 2,
    length(L1, N_half), % there is no need to compute both lists; L2 will be extracted from {1, 2, ..., N-1, N} - L1
    L1 #:: 1..N.

% auxiliary predicates
compute_squares([],[]).
compute_squares([H|L], [H * H|L1]) :-
    compute_squares(L, L1).

is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|L]) :-
    X #=< Y,
    is_sorted([Y|L]).

all_between(L, U, [L|X]):-
    L =< U,
    L1 is L + 1,
    all_between(L1, U, X).
all_between(L, U, []):-
    L > U.

set_difference([], _, []).
set_difference([H|T], L, [H|L1]):-
    \+member(H, L),
    set_difference(T, L, L1).
set_difference([H|T], L, L1):-
    member(H, L),
    set_difference(T, L, L1).
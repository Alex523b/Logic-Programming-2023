% Αλέξανδρος Ντιβέρης - 1115201900136
:- lib(ic).

numpart(N, L1, L2) :-
    N mod 2 =:= 0,
    N_half is N // 2,
    length(L1, N_half),
    L1 #:: 1..N,
    
    alldifferent(L1),
    ic_global:ordered_sum(L1, Sum),
    Sum #= (N*(N+1))/4,

    compute_squares(L1, L1_squared),
    sum(L1_squared) #= (N*(N+1)*(2*N+1))/12,

    find_first_element(L1, FirstElement),
    FirstElement #= 1,

    search(L1, 0, first_fail, indomain, complete, []),

    all_between(1, N, L3),
    findall(X, (member(X, L3), \+ member(X, L1)), L2).

compute_squares([],[]).
compute_squares([H|L], [H * H|L1]) :-
    compute_squares(L, L1).

find_first_element([H|_], H).

all_between(L, U, [L|X]):-
    L =< U,
    L1 is L + 1,
    all_between(L1, U, X).
all_between(L, U, []):-
    L > U.

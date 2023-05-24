% Αλέξανδρος Ντιβέρης - 1115201900136

:- lib(ic).
:- lib(branch_and_bound).
:- compile(randms).

maxsat(NV, NC, D, F, S, M) :-
    create_formula(NV, NC, D, F),

    length(S, NV),
    S #:: [0,1],

    findall(Y, (member(Y, F), Y \= []), UpdatedF), % empty clauses are always false; no need to evaluate them
    eval_clauses(UpdatedF, S, CostList),
    length(UpdatedF, LUF),
    Cost #= sum(CostList), % sum of CostList equals the number of clauses that cannot be evaluated as true (that's why their cost is non-zero)

    bb_min(search(S, 0, most_constrained, indomain, complete, []), Cost, bb_options{strategy:restart}),
    M is LUF - Cost. % To get the optimal number of clauses that can be evaluated as true, simply subtract the number of clauses with cost from the amount of non-empty clauses

eval_clauses([], _, []).
eval_clauses([Clause|RestClauses], S, [ClauseCost|RestCosts]) :-
    eval_clause(Clause, S, EvalClause),
    ClauseCost #= min(EvalClause),
    eval_clauses(RestClauses, S, RestCosts).

eval_clause([],_,[]).
eval_clause([H|T], S, [X|Rest]) :-
    X #:: [0,1],
    (H > 0 ->
        (get_var(H, S, Value), X #= 1 - Value)
    ;
        (abs(H, AbsH), get_var(AbsH, S, Value), X #= Value) % if X = 1 - Value, then ~X = Value
    ),
    eval_clause(T, S, Rest).

get_var(1, [Var|_], Var).
get_var(N, [_|T], Var) :-
    N > 0,
    UpdatedN is N - 1,
    get_var(UpdatedN, T, Var).
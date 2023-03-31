% Αλέξανδρος Ντιβέρης - 1115201900136

reverseListTillOperator(L, R, Operator) :- % Entry point to partly reversing a list based on specific element (Operator)
    reverseListTillOperator(L, [], R, Operator).
reverseListTillOperator([Operator|L], SoFarRev, ReversedList, Operator) :- % Base case: head of list is equal to the operator value
    append([Operator], SoFarRev, R),
    append(R, L, ReversedList).
reverseListTillOperator([X|L], SoFarRev, R, Operator) :- % Recursive call: traverse the list, while using SoFarRev as an accumulator to store the reversed list
    reverseListTillOperator(L, [X|SoFarRev], R, Operator).

pancakes_dfs(InitialState, Operators, States) :-
    dfs(InitialState, [InitialState], [], Operators, States).

dfs(CurrentState, States, Operators, Operators, States) :- % Final state
    is_sorted(CurrentState). 
dfs(State1, SoFarStates, SoFarOperators, Operators, States) :-
    reverseListTillOperator(State1, State2, Operator),
    \+ member(State2, SoFarStates),
    append(SoFarStates, [State2], NewSoFarStates),
    append(SoFarOperators, [Operator], NewSoFarOperators),
    dfs(State2, NewSoFarStates, NewSoFarOperators, Operators, States).

is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|L]) :-
    X =< Y,
    is_sorted([Y|L]).

pancakes_ids(InitialState, Operators, States) :-
    solve_pancakes_iter(0, InitialState, Op, _), % Firstly, find the minimum amount of moves required to arrange the pancakes,
    !, % Since we found the minimum amount of moves, there is no need to backtrack. Thus, cut backtracking.
    length(Op, MinLimit),
    solve_pancakes_fixed_limit(MinLimit, InitialState, Operators, States). % Find the different sequence of pancake flips with the minimum amount of moves required for the correct arrangement.

solve_pancakes_iter(Lim, InitialState, Operators, States) :-
    ldfs(Lim, InitialState, [InitialState], [], Operators, States).
solve_pancakes_iter(Lim, InitialState, Operators,  States) :-
    Lim1 is Lim + 1,
    solve_pancakes_iter(Lim1, InitialState, Operators, States).

solve_pancakes_fixed_limit(Lim, InitialState, Operators, States) :-
    ldfs(Lim, InitialState, [InitialState], [], Operators, States).

ldfs(_, CurrentState, States, Operators, Operators, States) :-
    is_sorted(CurrentState).
ldfs(Lim, State1, SoFarStates, SoFarOperators, Operators, States) :-
    Lim > 0,
    Lim1 is Lim - 1,
    reverseListTillOperator(State1, State2, Operator),
    \+ member(State2, SoFarStates),
    append(SoFarStates, [State2], NewSoFarStates),
    append(SoFarOperators, [Operator], NewSoFarOperators),
    ldfs(Lim1, State2, NewSoFarStates, NewSoFarOperators, Operators, States).
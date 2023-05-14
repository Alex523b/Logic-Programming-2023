dimension(5).
black(1,3).
black(2,3).
black(3,2).
black(4,3).
black(5,1).
black(5,5).

split_words_in_row([], [[]]).    
split_words_in_row([###|T], [[]|T2]) :-
    split_words_in_row(T, T2).
split_words_in_row([H|T], [[H|T2]|T3]) :-
    H \= ###,
    split_words_in_row(T, [T2|T3]).

transpose(_, I, []) :-
    dimension(M),
    I > M.
transpose(Matrix, I, [Column|T]) :-
    UpdatedI is I + 1,
    nth_column(Matrix, I, Column),
    transpose(Matrix, UpdatedI, T).

make_matrix(Matrix) :-
    dimension(M),
    length(Matrix, M),
    make_rows(M, Matrix),
    
    find_variables(Matrix, 1, TempRowVariables),
    filter_variables(TempRowVariables, RowVariables),

    transpose(Matrix, 1, TransposedMatrix),
    find_variables(TransposedMatrix, 1, TempColumnVariables),
    filter_variables(TempColumnVariables, ColumnVariables),

    append(RowVariables, ColumnVariables, Variables),
    printM(Matrix),
    writeln(RowVariables),
    writeln(ColumnVariables).

filter_variables(Variables, UpdatedColumnWords) :-
    remove_empty_vars(Variables, TempColumnWords),
    flatten_list(TempColumnWords, UpdatedColumnWords).

flatten_list([], []).
flatten_list([[T|T1]|Rest], [T|Bla]) :- !,
    flatten_list([T1|Rest], Bla).
flatten_list([T|Rest], Bla) :-
    length(T, L),
    L =< 1,
    flatten_list(Rest, Bla).

remove_empty_vars([], []).
remove_empty_vars([[]|Rest], Bla) :-
    !,
    remove_empty_vars(Rest, Bla).
remove_empty_vars([T|Rest], [T|Bla]) :-
    remove_empty_vars(Rest, Bla).

find_variables(_, I, []) :-
    dimension(M),
    I > M.
find_variables(Matrix, I, [WordsInRow|Words]) :-
    nth_row(Matrix, I, Row),
    split_words_in_row(Row, SplitRow),
    remove_one_character_word(SplitRow, WordsInRow),
    UpdatedI is I + 1,
    find_variables(Matrix, UpdatedI, Words).

remove_one_character_word([], []).
remove_one_character_word([T|Rest], NewL) :-
    length(T, L),
    L =< 1, !,
    remove_one_character_word(Rest, NewL).
remove_one_character_word([T|Rest], [T|NewL]) :-
    remove_one_character_word(Rest, NewL).

nth_row([H|_], 1, H) :- !.
nth_row([_|T], I, X) :-
    I1 is I - 1,
    nth_row(T, I1, X).

nth_column([], _, []).
nth_column([H|T], I, [R|X]) :-
    nth_row(H, I, R), 
    nth_column(T, I, X).

printM([]).
printM([H|L]) :-
    writeln(H),
    printM(L).

make_rows(_, []).
make_rows(M, [Row|Matrix]) :-
    length(InitialRow, M),
    length(Matrix, K),
    RowIndex is M - K, % each row is computed recursively; that is, in reverse. Thus, with M - K we avoid reversing the matrix afterwards
    fill_row(InitialRow, RowIndex, Row),
    make_rows(M, Matrix).

fill_row([], _, []).
fill_row([_|T], RowIndex, [###|Rest]) :-
    dimension(M),
    length(T, L),
    ColumnIndex is M - L, % same reason with row index computation
    black(RowIndex, ColumnIndex), !,
    fill_row(T, RowIndex, Rest).
fill_row([X-RowIndex-ColumnIndex|T], RowIndex, [X-RowIndex-ColumnIndex|Rest]) :-
    dimension(M),
    length(T, L),
    ColumnIndex is M - L, % same reason with row index computation
    fill_row(T, RowIndex, Rest).

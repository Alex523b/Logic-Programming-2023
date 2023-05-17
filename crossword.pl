% Αλέξανδρος Ντιβέρης - 1115201900136

crossword(S) :-
    compute_matrix(Matrix),
    find_variables(Matrix, 1, TempRowVariables),
    filter_variables(TempRowVariables, RowVariables),

    transpose(Matrix, 1, TransposedMatrix),
    find_variables(TransposedMatrix, 1, TempColumnVariables),
    filter_variables(TempColumnVariables, ColumnVariables),

    append(RowVariables, ColumnVariables, Variables),
    findall(Word, words(Word), Words),
    flatten_list(Words, FlattedWords),
    solution(Variables, FlattedWords, S),

    print_crossword(Matrix), !.

solution([], _, []).
solution([X|Others], Words, [Word|Rest]) :-
    solution(Others, Words, Rest),
    choose_word(Words, Word),
    valid_choice(Word, X).

choose_word(Words, Word) :-
    member(Word, Words).

valid_choice(Word, X) :-
    remove_index_info(X, Xs),
    name(Word, Xs).

compute_matrix(Matrix) :-
    dimension(M),
    length(Matrix, M),
    make_rows(M, Matrix).

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

transpose(_, I, []) :-
    dimension(M),
    I > M.
transpose(Matrix, I, [Column|T]) :- % the rows of the transposed matrix are the columns of the original one
    UpdatedI is I + 1,
    nth_column(Matrix, I, Column),
    transpose(Matrix, UpdatedI, T).

print_crossword([]).
print_crossword([H|L]) :-
    print_row(H),
    writeln(''),
    print_crossword(L).

print_row([]).
print_row([###|Rest]) :-
    printf("###", []),
    print_row(Rest).
print_row([X-_-_|Rest]) :-
    printf(" %c ", [X]),
    print_row(Rest).

nth_row([H|_], 1, H) :- !.
nth_row([_|T], I, X) :-
    I1 is I - 1,
    nth_row(T, I1, X).

nth_column([], _, []).
nth_column([H|T], I, [R|X]) :-
    nth_row(H, I, R), 
    nth_column(T, I, X).

find_variables(_, I, []) :-
    dimension(M),
    I > M.
find_variables(Matrix, I, [Variables|RestVariables]) :-
    nth_row(Matrix, I, Row),
    split_variables_in_row(Row, TempVariables),
    remove_vars_of_length_one(TempVariables, Variables), % each word consists of at least 2 letters
    UpdatedI is I + 1,
    find_variables(Matrix, UpdatedI, RestVariables).

filter_variables(Variables, FilteredVariables) :-
    remove_empty_vars(Variables, TempVariables), % discard empty sublists
    flatten_list(TempVariables, FilteredVariables). % there is no need to separate variables based on where they lie in the matrix

split_variables_in_row([], [[]]).    
split_variables_in_row([###|T], [[]|T2]) :-
    !,
    split_variables_in_row(T, T2).
split_variables_in_row([H|T], [[H|T2]|T3]) :-
    split_variables_in_row(T, [T2|T3]).

remove_index_info([], []).
remove_index_info([H-_-_|T], [H|Rest]) :-
    remove_index_info(T,Rest).

flatten_list([], []).
flatten_list([[H|T]|T1], [H|Rest]) :-
    !,
    flatten_list([T|T1], Rest).
flatten_list([H|T], List) :-
    length(H, N),
    N =< 1,
    flatten_list(T, List).

remove_empty_vars([], []).
remove_empty_vars([[]|T], Rest) :-
    !,
    remove_empty_vars(T, Rest).
remove_empty_vars([H|T], [H|Rest]) :-
    remove_empty_vars(T, Rest).

remove_vars_of_length_one([], []).
remove_vars_of_length_one([T|Rest], L) :-
    length(T, N),
    N =< 1,
    remove_vars_of_length_one(Rest, L).
remove_vars_of_length_one([T|Rest], [T|L]) :-
    remove_vars_of_length_one(Rest, L).

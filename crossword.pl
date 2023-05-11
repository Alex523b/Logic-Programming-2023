dimension(5).

black(1,3).
black(2,3).
black(3,2).
black(4,3).
black(5,1).
black(5,5).

words([adam,al,as,do,ik,lis,ma,oker,ore,pirus,po,so,ur]).

make_matrix(Matrix) :-
    dimension(M),
    length(Matrix, M),
    make_lines(M, Matrix),
    printM(Matrix).

printM([]).
printM([H|L]) :-
    writeln(H),
    printM(L).

make_lines(_, []).
make_lines(M, [Row|Matrix]) :-
    length(Line, M),
    length(Matrix, K),
    RowIndex is M - K,
    fill_row(Line, RowIndex, Row),
    make_lines(M, Matrix).

fill_row([], _, []).
fill_row([_|T], RowIndex, [###|Rest]):-
    dimension(M),
    length(T, L),
    ColumnIndex is M - L,
    black(RowIndex, ColumnIndex), !,
    fill_row(T, RowIndex, Rest).
fill_row([_|T], RowIndex, [_|Rest]):-
    fill_row(T, RowIndex, Rest).
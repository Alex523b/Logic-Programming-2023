mymember(X, [X|_]).
mymember(X, [_ | L]):- mymember(X, L).

% generator: ? member(X, [a, b, c]).
% X = a; X = b; X = c;

% ?append([a, b, c], [d, e], L). L = [a, b, c, d, e]
myappend([], []).
myappend([X|L1], L2, [X|L3]):- myappend(L1, L2, L3).

mylast(L, X):- append(_, [X], L).

mylast2([X], X).
mylast2([_|L], X):- mylast2(L, X).

first([X|_], X). % no need for recursion. the first element of our list is right in front of us

add_last(L1, X, L2):- append(L1, [X], L2).

add_last2([], X, [X]).
add_last2([Y|L1], X, [Y|L2]):-add_last(L1, X, L2).

add_first(L1, X, L2):-append([X], L1, L2).

add_first2(L1, X, [X|L1]).

evenlength([]).
evenlength([_,_|L]):-evenlength(L).

oddlength([_]).
oddlength([_, _|L]):- oddlength(L).

% alternative
evenlength2([]).
evenlength2([_|L]):-oddlength2(L).

oddlength2([_|L]):-evenlength(L).

myreverse([], []).
myreverse([X|L], L2):- myreverse(L, RL), append(RL, [X], L2). % O(n^2) time complexity

myreverse2(L, R):-myreverse2(L, [], R).
myreverse2([], R, R).
myreverse2([X|L], SoFarRev, R):-myreverse2(L, [X|SoFarRev], R). % SoFarRev: accumulator, O(n) time complexity

mypalindrome(L):-reverse(L, L).
mypalindrome([]). 
% same base cases apply for mypalindrome2
mypalindrome([_]).
mypalindrome2([X|L]):-append(L1, [X], L), mypalindrome2(L1).

compute(L):-findAllBetweenBlaAndFoo(L, L1).
findAllBetweenBlaAndFoo(L, L1):- append(_,[bla|L2], L), append(L1, [foo|_], L2).

mydelete(X, [X|L], L).
mydelete(X, [Y|L1], [Y|L2]):-mydelete(X, L1, L2).

% delete(a, L, [1,2,3,4]) -> non deterministic insert where a is added to L in every position of the list

insert(X, List, BiggerList) :- delete(X, BiggerList, List).

permutation([],[]).
permutation([X|L], P):-
    permutation(L, L1),
    insert(X, L1, P).

foo([], _, []).
foo([H|L], [_ - Y - Z|T], [H-Y-Z|List]) :-
    foo(L, T, List).

permutation2([],[]).
permutation2(L, [X|P]):-
    delete(X, L, L1),
    permutation2(L1, P).

sublist([], _).
sublist(S, L):-
    append(_, L1, L),
    append(S, _, L1),
    S \= [].
    
gensubset([],[]).
gensubset([X|S], [X|L]):-
    gensubset(S,L).
gensubset(S, [_|L]):-
    gensubset(S, L).

mylength([],0).
mylength([_|L], N):-
    mylength(L, M),
    N is M + 1.
    
% alternative method for calculating list length (less expensive space-wise)
mylength1(L, N):-
    myLength2(L,0,N).
mylength2([], SoFar, SoFar).
mylength2([_|L], SoFar, N):-
    NewSoFar is SoFar + 1,
    mylength2(L, NewSoFar, N).

mylength3([], 0). % predicate definition resembling the built-in length predicate
mylength3([_|L], N):-
    N > 0,
    M is N - 1,
    mylength3(L, M).
    
my_flatten([], []).
my_flatten([L1|L2], FL):-
	my_flatten(L1, FL1),
	my_flatten(L2, FL2),
	append(FL1, FL2, FL).
my_flatten(X,[X]).

fixed_flatten([], []):- !.
fixed_flatten([L1|L2], FL) :- !,
	fixed_flatten(L1, FL1),
	fixed_flatten(L2, FL2),
	append(FL1, FL2, FL).

fixed_flatten(X, [X]).

%	\+ Goal:- % implementing negation as failure (\+ predicate)
%	call(Goal), !, fail.
%	\+ Goal.

my_intersection([], _, []).
my_intersection([X|L1], L2, [X|L3]):-
	member(X, L2),
	my_intersection(L1, L2, L3).
my_intersection([X|L1], L2, L3):-
	\+ member(X, L2),
	my_intersection(L1, L2, L3).

intersection2([], _, []).
intersection2([X|L1], L2, [X|L3]):-
	member(X, L2), !,
	intersection2(L1, L2, L3).
intersection2([_|L1], L2, L3):-
	intersection2(L1, L2, L3).

between(L, U, L):-
	L =< U.
between(L, U, X):-
	L < U,
	L1 is L + 1,
	between(L1, U, X).

clear_dups(L, CL):-
	clear_dups(L, [], CL).
clear_dups([], CL, CL).
clear_dups([X|L], L1, CL):-
	member(X, L1),
	clear_dups(L, L1, CL).
clear_dups([X|L], L1, CL):-
	\+member(X, L1),
	clear_dups(L, [X|L1], CL).

my_union([], L1, L1).
my_union(L1, L2, L4):-
	append(L1, L2, L3), clear_dups(L3, L4).

my_union2(L1, L2, L3):-
    my_union2(L1, L2, L1, L3).
my_union2(_, [], L, L).
my_union2(_, [X|L1], Y, L2):-
    \+member(X, Y), !,
    append(Y, [X], Y1), 
    my_union2(_, L1, Y1, L2).
my_union2(_, [_|L1], L, L2):-
    my_union2(_, L1, L, L2).

set_difference([], _, []).
set_difference([H|T], L, [H|L1]):-
	\+member(H, L),
	set_difference(T, L, L1).
set_difference([H|T], L, L1):-
	member(H, L),
	set_difference(T, L, L1).

all_between(L, U, [L|X]):-
	L =< U,
	L1 is L + 1,
	all_between(L1, U, X).
all_between(L, U, []):-
	L > U.

alt_all_between(L, U, X):-
	findall(Y, between(L,U,Y), X).

alt_between(L, U, X):-
	all_between(L, U, Y),
	member(X, Y).

is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|L]):-
	X =< Y,
	is_sorted([Y|L]).

% Two sets are equal when they have the same cardinality and they contain the same elements
set_equality([], []).
set_equality([H1|T1], [H2|T2]) :-
    member(H1, [H2|T2]),
    member(H2, [H1|T1]),
    list_equality(T1, T2).
    
alt_set_equality(S1, S2) :-
    length(S1, N_S1),
    length(S2, N_S2),
    N_S1 =:= N_S2,
    intersection(S1, S2, S1).

get_unifiable([], _, []).
get_unifiable([X|List1], Y, [X|List2]) :-
    \+ X \= Y,
    !,
    get_unifiable(List1, Y, List2).
get_unifiable([_|List1], Y, List2) :-
    get_unifiable(List1, Y, List2).
    
get_same([], _, []).
get_same([X|List1], Y, [X|List2]) :-
    X == Y, % X = 5, X == 5 -> Yes | X == 5 -> No
    !,
    get_same(List1, Y, List2).
get_same([_|List1], Y, List2) :-
    get_same(List1, Y, List2).
    
% X = 5 ισοτητα ενοποιησης
% αναθεση αριθμητικης εκφρασης: Χ is ...
% ισοτητα τιμων αριθμητικων εκφρασεων X =:= 5, Χ =\= 5
% ισοτητα κατα λεξη Χ == Y -> No (X \== Y)
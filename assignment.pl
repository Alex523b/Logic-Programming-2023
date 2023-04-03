% Αλέξανδρος Ντιβέρης - 1115201900136
activity(a01, act(0,3)).
activity(a02, act(0,4)).
activity(a03, act(1,5)).
activity(a04, act(4,6)).
activity(a05, act(6,8)).
activity(a06, act(6,9)).
activity(a07, act(9,10)).
activity(a08, act(9,13)).
activity(a09, act(11,14)).
activity(a10, act(12,15)).
activity(a11, act(14,17)).
activity(a12, act(16,18)).
activity(a13, act(17,19)).
activity(a14, act(18,20)).
activity(a15, act(19,20)).

assignment(NPersons, Assignment) :-
	findall(Aid, activity(Aid, _), AIds), % Gather all activities in list AIds
	assign(AIds, NPersons, Assignment).
assign([], _, []).
assign([AId|AIds], NPersons, [AId-PId|Assignment]) :-
assign(AIds, NPersons, Assignment),
	between(1, NPersons, PId), % Select a person PId for activity AId
	activity(AId, act(Ab, Ae)),
	gather(PId, Assignment, APIds), % Gather in list APIds so far activities of PId
	valid(Ab, Ae, APIds). % Is current assignment consistent with previous ones?

valid(_, _, []).
valid(Ab1, Ae1, [APId|APIds]) :-
	activity(APId, act(Ab2, Ae2)),
	Ab2 > Ae1, % fill
	valid(Ab1, Ae1, APIds).

all_between(L, U, [L|X]):-
	L =< U,
	L1 is L + 1,
	all_between(L1, U, X).
all_between(L, U, []):-
	L > U.

between(L, U, L):-
	L =< U.
between(L, U, X):-
	L < U,
	L1 is L + 1,
	between(L1, U, X).

gather(PId, Assignment, AIds):-
    gather(PId, Assignment, [], AIds).
gather(PId, [AId-PId|L], T, AIds):-
    gather(PId, L, [AId|T], AIds).
gather(PId, [_-PId2|L], T, AIds):-
    PId \= PId2,
    gather(PId, L, T, AIds).
gather(_, [], AIds, AIds).
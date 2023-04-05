% Αλέξανδρος Ντιβέρης - 1115201900136
activity(1, act(0,3)). activity(2, act(0,4)). activity(3, act(1,5)).
activity(4, act(4,6)). activity(5, act(6,8)). activity(6, act(6,9)).
activity(7, act(9,10)). activity(8, act(9,13)). activity(9, act(11,14)).

assignment(NPersons, Assignment) :-
	findall(Aid, activity(Aid, _), AIds), % gatherSoFarActivities all activities in list AIds
	assign(AIds, NPersons, Assignment).
assign([], _, []).
assign([AId|AIds], NPersons, [AId-PId|Assignment]) :-
assign(AIds, NPersons, Assignment),
	between(1, NPersons, PId), % Select a person PId for activity AId
	activity(AId, act(Ab, Ae)),
	gatherSoFarActivities(PId, Assignment, APIds), % Gather in list APIds so far activities of PId
	valid(Ab, Ae, APIds). % Is current assignment consistent with previous ones?

valid(_, _, []).
valid(Ab1, Ae1, [APId|APIds]) :-
	activity(APId, act(Ab2, Ae2)),
	Ab2 > Ae1, % fill
	valid(Ab1, Ae1, APIds).

between(L, U, L):-
	L =< U.
between(L, U, X):-
	L < U,
	L1 is L + 1,
	between(L1, U, X).

gatherSoFarActivities(_, [], []).
gatherSoFarActivities(PId, [_-PId2|L], T) :-
    PId =\= PId2,
    gatherSoFarActivities(PId, L, T).
gatherSoFarActivities(PId, [AId-PId|L], [AId|T]) :-
    gatherSoFarActivities(PId, L, T).
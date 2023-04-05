% Αλέξανδρος Ντιβέρης - 1115201900136
activity(1, act(0,3)). activity(2, act(0,4)). activity(3, act(1,5)).
activity(4, act(4,6)). activity(5, act(6,8)). activity(6, act(6,9)).
activity(7, act(9,10)). activity(8, act(9,13)). activity(9, act(11,14)).

assignment(NPersons, Assignment, ASP) :-
	findall(Aid, activity(Aid, _), AIds), % gatherSoFarActivities all activities in list AIds
    computeInitialASP(NPersons, ASP),
	assign(AIds, NPersons, Assignment, ASP).

assign([], _, [], _).
assign([AId|AIds], NPersons, [AId-PId|Assignment], ASP) :-
	between(1, NPersons, PId), % Select a person PId for activity AId
    updateElement(PId, AId, ASP, NewASP),
    assign(AIds, NPersons, Assignment, NewASP),
	gatherSoFarActivities(PId, Assignment, APIds), % Gather in list APIds so far activities of PId
	activity(AId, act(Ab, Ae)),
	valid(Ab, Ae, APIds). % Is current assignment consistent with previous ones?

valid(_, _, []).
valid(Ab1, Ae1, [APId|APIds]) :-
	activity(APId, act(Ab2, Ae2)),
	Ab2 > Ae1, % fill
	valid(Ab1, Ae1, APIds).

between(L, U, L) :-
	L =< U.
between(L, U, X) :-
	L < U,
	L1 is L + 1,
	between(L1, U, X).

all_between(L, U, [L|X]) :-
	L =< U,
	L1 is L + 1,
	all_between(L1, U, X).
all_between(L, U, []) :-
	L > U.

gatherSoFarActivities(_, [], []).
gatherSoFarActivities(PId, [_-PId2|L], T) :-
    PId =\= PId2,
    gatherSoFarActivities(PId, L, T).
gatherSoFarActivities(PId, [AId-PId|L], [AId|T]) :-
    gatherSoFarActivities(PId, L, T).

computeInitialASP(NPersons, InitializedASP) :-
    all_between(1, NPersons, InitialASP),
    fillInitialASP(InitialASP, InitializedASP).

fillInitialASP([], []).
fillInitialASP([PId|L], [PId - [] - 0 | T]) :-
    fillInitialASP(L, T).

updateElement(PId, AId, L, UpdatedL) :-
    updateElement(PId, AId, L, [], TempUpdatedL),
    reverse(TempUpdatedL, UpdatedL).
updateElement(_, _, [], L, L).
updateElement(PId, AId, [PId - X - Y | T], SoFarL, UpdatedL) :-
    append(X, [AId], X2),
    activity(AId, act(B, E)),
    NewDuration is Y + E - B,
    updateElement(PId, AId, T, [PId - X2 - NewDuration | SoFarL], UpdatedL).
updateElement(PId, AId, [PId2 - X - Y | T], SoFarL, UpdatedL) :-
    PId =\= PId2,
    updateElement(PId, AId, T, [PId2 - X - Y | SoFarL], UpdatedL).
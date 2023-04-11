% Αλέξανδρος Ντιβέρης - 1115201900136

assignment(NP, MT, ASP, ASA) :-
    findall(Aid, activity(Aid, _), AIds), % Gather all activities in list AIds
    assign(AIds, NP, MT, ASA),
    compute_initial_ASP(NP, InitialASP),
    compute_final_ASP(ASA, InitialASP, ASP).

assign([], _, _, []).
assign([AId|AIds], NP, MT, [AId-PId|Assignment]) :-
    assign(AIds, NP, MT, Assignment),
    between(1, NP, PId), % Select a person PId for activity AId
    activity(AId, act(Ab, Ae)),
    findall(X, member(X-PId, Assignment), APIds), % Gather in list APIds so far activities of PId
    valid(Ab, Ae, APIds, 0, MT),
    check_duplicate(PId, Assignment, 1, APIds, Ab, Ae, MT).% Is current assignment consistent with previous ones?

total_time([], Sum, Sum).
total_time([APId|APIds], TmpSum, Sum) :-
    activity(APId, act(As, Ae)),
    NewTmpSum is TmpSum + Ae - As,
    total_time(APIds, NewTmpSum, Sum).

check_duplicate(PId, _, PId, _, _, _, _).
check_duplicate(PId, Assignment, PId2, APIds, Ab, Ae, MT) :-
    findall(X, member(X-PId2, Assignment), APIds2),
    (\+ valid(Ab, Ae, APIds2, 0, MT);
    total_time(APIds, 0, Sum),
    total_time(APIds2, 0, Sum2),
    Sum =\= Sum2), !,
    NewPId2 is PId2 + 1,
    check_duplicate(PId, Assignment, NewPId2, APIds, Ab, Ae, MT).

valid(As1, Ae1, [], Sum, MT) :-
    Sum + Ae1 - As1 =< MT. % The sum of time units allocated for activities cannot be higher than the maximum time.
valid(As1, Ae1, [APId|APIds], Sum, MT) :-
    activity(APId, act(As2, Ae2)),
    (As2 > Ae1; As1 > Ae2), % Assure that there is no overlap between assigned activities
    UpdatedSum is Sum + Ae2 - As2,
    valid(As1, Ae1, APIds, UpdatedSum, MT).
    
update_ASP(PId, AId, L, UpdatedL) :-
    update_ASP(PId, AId, L, [], TempUpdatedL),
    reverse(TempUpdatedL, UpdatedL).
update_ASP(_, _, [], L, L).
update_ASP(PId, AId, [PId - X - Y|T], SoFarL, UpdatedL) :- % Append information of assigned activity to final ASP
    activity(AId, act(S, E)),
    NewDuration is Y + E - S,
    update_ASP(PId, AId, T, [PId - [AId|X] - NewDuration|SoFarL], UpdatedL).
update_ASP(PId, AId, [PId2 - X - Y|T], SoFarL, UpdatedL) :-
    PId =\= PId2,
    update_ASP(PId, AId, T, [PId2 - X - Y|SoFarL], UpdatedL).

compute_initial_ASP(NP, InitializedASP) :- % Before assigning persons to activities, the ASP has the following format: [PId - [] - 0, PId2 - [] - 0, ..., PIdNP - [] - 0]
    all_between(1, NP, InitialASP),
    fill_initial_ASP(InitialASP, InitializedASP).

fill_initial_ASP([], []).
fill_initial_ASP([PId|L], [PId - [] - 0|T]) :-
    fill_initial_ASP(L, T).

compute_final_ASP([], L, L).
compute_final_ASP([AId - PId|RestAssignedActivities], InitialASP, ASP) :-
    update_ASP(PId, AId, InitialASP, UpdatedASP),
    compute_final_ASP(RestAssignedActivities, UpdatedASP, ASP).

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
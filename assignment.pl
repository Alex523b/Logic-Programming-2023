% Αλέξανδρος Ντιβέρης - 1115201900136

% REMOVE THEM BEFORE SUMBITTING
 activity(a1, act(0,3)).
 activity(a2, act(4,6)).
 activity(a3, act(1,2)).

assignment(NP, MT, ASP, ASA) :-
    findall(Aid, activity(Aid, _), AIds), % gather_so_far_activities all activities in list AIds
    assign(AIds, NP, MT, ASA),
    compute_initial_ASP(NP, InitialASP),
    compute_final_ASP(ASA, InitialASP, ASP).

compute_final_ASP([], L, L).
compute_final_ASP([AId - PId|Rest], InitialASP, ASP) :-
    update_element(PId, AId, InitialASP, NewASP),
    compute_final_ASP(Rest, NewASP, ASP).

compute_initial_ASP(NP, InitializedASP) :-
    all_between(1, NP, InitialASP),
    fill_initial_ASP(InitialASP, InitializedASP).

fill_initial_ASP([], []).
fill_initial_ASP([PId|L], [PId - [] - 0|T]) :-
    fill_initial_ASP(L, T).

assign([], _, _, []).
assign([AId|AIds], NP, MT, [AId-PId|Assignment]) :-
    assign(AIds, NP, MT, Assignment),
    between(1, NP, PId), % Select a person PId for activity AId
    activity(AId, act(Ab, Ae)),
    gather_so_far_activities(PId, Assignment, APIds), % Gather in list APIds so far activities of PId
    valid(Ab, Ae, APIds, 0, MT). % Is current assignment consistent with previous ones?

valid(As1, Ae1, [], Sum, MT) :-
    Sum + Ae1 - As1 =< MT.
valid(As1, Ae1, [APId|APIds], Sum, MT) :-
    activity(APId, act(As2, Ae2)),
    (As2 > Ae1 ; As1 > Ae2),
    UpdatedSum is Sum + Ae2 - As2,
    valid(As1, Ae1, APIds, UpdatedSum, MT).

gather_so_far_activities(_, [], []).
gather_so_far_activities(PId, [_-PId2|L], T) :-
    PId =\= PId2,
    gather_so_far_activities(PId, L, T).
gather_so_far_activities(PId, [AId-PId|L], [AId|T]) :-
    gather_so_far_activities(PId, L, T).

update_element(PId, AId, L, UpdatedL) :-
    update_element(PId, AId, L, [], TempUpdatedL),
    reverse(TempUpdatedL, UpdatedL).
update_element(_, _, [], L, L).
update_element(PId, AId, [PId - X - Y|T], SoFarL, UpdatedL) :-
    activity(AId, act(B, E)),
    NewDuration is Y + E - B,
    update_element(PId, AId, T, [PId - [AId|X] - NewDuration|SoFarL], UpdatedL).
update_element(PId, AId, [PId2 - X - Y|T], SoFarL, UpdatedL) :-
    PId =\= PId2,
    update_element(PId, AId, T, [PId2 - X - Y|SoFarL], UpdatedL).

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
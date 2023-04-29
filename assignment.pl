% Αλέξανδρος Ντιβέρης - 1115201900136
/*activity(a1, act(0,3)).
activity(a2, act(4,6)).
activity(a3, act(1,2)).*/

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

assignment(NP, MT, ASP, ASA) :-
    findall(Aid, activity(Aid, _), AIds), % Gather all activities in list AIds
    assign(AIds, NP, MT, ASA),
    compute_ASP(ASA, 1, NP, ASP).

compute_ASP(_, PId, NP, []):-
    PId > NP.
compute_ASP(ASA, PId, NP, [PId-APIds-Sum|Rest]):-
    NewPId is PId + 1,
    PId =< NP,
    findall(AId, member(AId-PId, ASA), APIds),
    total_time(APIds, Sum),
    compute_ASP(ASA, NewPId, NP, Rest).

assign([], _, _, []).
assign([AId|AIds], NP, MT, [AId-PId|Assignment]) :-
    assign(AIds, NP, MT, Assignment),
    between(1, NP, PId), % Select a person PId for activity AId
    activity(AId, act(Ab, Ae)),
    findall(X, member(X-PId, Assignment), APIds), % Gather in list APIds so far activities of PId
    valid(Ab, Ae, APIds, 0, MT), % Is current assignment consistent with previous ones?
    check_duplicate(PId, Assignment, 1, APIds, Ab, Ae, MT).

total_time([], Sum, Sum).
total_time([APId|APIds], TmpSum, Sum) :-
    activity(APId, act(As, Ae)),
    NewTmpSum is TmpSum + Ae - As,
    total_time(APIds, NewTmpSum, Sum).

check_duplicate(PId, _, PId, _, _, _, _).
check_duplicate(PId, Assignment, PId2, APIds, Ab, Ae, MT) :-
    findall(X, member(X-PId2, Assignment), APIds2),
    % If a person with pid smaller than PId can handle an assignment, then consider this solution duplicate
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
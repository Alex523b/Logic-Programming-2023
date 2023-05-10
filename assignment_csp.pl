% Αλέξανδρος Ντιβέρης - 1115201900136

:- lib(ic).
/*activity(a1, act(0,3)).
activity(a2, act(1,2)).
activity(a3, act(4,6)).*/

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

bla([]).
bla([AId-PId|AIds]) :-
    constraint(AId-PId, AIds),
    bla(AIds).

constraint(_, []).
constraint(AId-PId, [AId2-PId2|Rest]) :-
    activity(AId, act(S, E)),
    activity(AId2, act(S2, E2)),
    ((S > E2 ; S2 < E) -> PId #\= PId2 ; true),
    constraint(AId-PId, Rest).

get_pids([], []).
get_pids([_-PId|Rest], [PId|T]) :-
    get_pids(Rest, T).

total_time(Xs, Sum) :-
    total_time(Xs, [], Sum).

total_time([], Sum, Sum).
total_time([H|T], TmpSum, Sum) :-
    activity(H, act(S,E)),
    Duration is E - S,
    total_time(T, [Duration|TmpSum], Sum).
assignment_csp(NP, MT, ASP, ASA) :-
    findall(AId, activity(AId, _), AIds),
    compute_initial_ASA(AIds, NP, ASA),
    
    bla(ASA),
    get_pids(ASA, PIds),
    findall(X, member(X-2, ASA), Xs),
    total_time(Xs, Sum),
    writeln(Sum),
    sum(Sum) #=< MT,
    findall(X, member(X-1, ASA), Xs),
    total_time(Xs, Sum2),
    writeln(Sum2),
    sum(Sum2) #=< MT,
    labeling(PIds),
    compute_initial_ASP(NP, InitializedASP),
    compute_final_ASP(ASA, InitializedASP, ASP).

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

compute_initial_ASA([], _, []).
compute_initial_ASA([AId|AIds], NP, [AId-PId|T]) :-
    PId #:: 1..NP,
    compute_initial_ASA(AIds, NP, T).

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
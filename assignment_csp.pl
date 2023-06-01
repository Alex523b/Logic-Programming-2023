% Αλέξανδρος Ντιβέρης - 1115201900136

:- lib(ic).
:- lib(branch_and_bound).
:- compile(activity).

assignment_csp(NP, ST, ASP, ASA) :-
    findall(AId, activity(AId,_), Activities),
    length(Activities, NumberOfActivities),

    length(Solution, NumberOfActivities),
    Solution #:: 1..NP,

    make_tmpl(Activities, Solution, Assignments),
    compute_average_duration(NP, Activities, AverageDuration),
    assign(NumberOfActivities, NP, ST, AverageDuration, Assignments, Costs),

    compute_ASA(Assignments, [], ASA),
    compute_ASP(NP, Assignments, ASP),

    search(Solution, 0, most_constrained, indomain_middle, complete, []).

assignment_opt(NF, NP, MT, F, T, ASP, ASA) :-
    findall(AId, activity(AId, _), Activities),
    (NF > 0 ->
        fill_activities(NF, Activities, ActivitiesForAssignment)
        ;
        ActivitiesForAssignment = Activities
    ).
    %bb_min(search(Solution, 0, first_fail, indomain, complete, []), Cost, bb_options{timeout: T}).

make_tmpl([], [], []).
make_tmpl([AId|RestOfActivities], [PId|RestPersons], [AId - PId|RestAssignedActivites]) :-
    make_tmpl(RestOfActivities, RestPersons, RestAssignedActivites).

compute_average_duration(NP, Activities, AverageDuration) :-
    compute_duration(Activities, 0, D),
    AverageDuration is round(D / NP).

compute_duration([], D, D).
compute_duration([AId|RestDuration], Time, D) :-
    activity(AId, act(S,E)),
    Duration is E - S,
    NewTime is Time + Duration,
    compute_duration(RestDuration, NewTime, D).

assign(_, _, _, _, [], []).
assign(Position, NP, ST, A, [AId-PId|RestAssignedActivities], Costs) :-
    NewPosition is Position - 1,
    assign(NewPosition, NP, ST, A, RestAssignedActivities, Costs),
    activity(AId, act(S,E)),
    (Position < NP -> 
        (constrain_assignment(Position, S, E, 0, ST, RestAssignedActivities), PId #= Position)
        ;
        (constrain_assignment(PId, S, E, 0, ST, RestAssignedActivities))
    ).

constrain_assignment(_, S, E, Time, ST, []) :-
    Duration is E - S,
    NewTime is Time + Duration,
    NewTime =< ST.
constrain_assignment(PId, S, E, Time, ST, [_-Selected|RestAssignedActivities]) :-
    PId #\= Selected,
    constrain_assignment(PId, S, E, Time, ST, RestAssignedActivities).
constrain_assignment(PId, S, E, Time, ST, [AId-Selected|RestAssignedActivities]) :-
    PId #= Selected,
    activity(AId, act(S1, E1)),
    E < S1,
    Duration is E1 - S1,
    NewTime is Time + Duration,
    NewTime =< ST,
    constrain_assignment(PId, S, E, NewTime, ST, RestAssignedActivities).

compute_ASA([], ASA, ASA).
compute_ASA([AId-PId|RestAssigned], SoFarASA, ASA) :-
    append(SoFarASA, [AId-PId], NewSoFarASA),
    compute_ASA(RestAssigned, NewSoFarASA, ASA).

compute_ASP(NP, Assignments, ASP) :-
    compute_initial_ASP(NP, InitializedASP),
    compute_final_ASP(Assignments, InitializedASP, ASP).

compute_initial_ASP(NP, InitializedASP) :- % Before assigning persons to activities, the ASP has the following format: [PId - [] - 0, PId2 - [] - 0, ..., PIdNP - [] - 0]
    all_between(1, NP, InitialASP),
    fill_initial_ASP(InitialASP, InitializedASP).

all_between(L, U, [L|X]) :-
    L =< U,
    L1 is L + 1,
    all_between(L1, U, X).
all_between(L, U, []) :-
    L > U.

% the following predicates have also been used in the original assignment problem

fill_initial_ASP([], []).
fill_initial_ASP([PId|L], [PId - [] - 0|T]) :-
    fill_initial_ASP(L, T).

compute_final_ASP([], L, L).
compute_final_ASP([AId-PId|RestAssignedActivities], InitialASP, ASP) :-
    update_ASP(PId, AId, InitialASP, UpdatedASP),
    compute_final_ASP(RestAssignedActivities, UpdatedASP, ASP).

update_ASP(PId, AId, L, UpdatedL) :-
    update_ASP(PId, AId, L, [], TempUpdatedL),
    reverse(TempUpdatedL, UpdatedL).
update_ASP(_, _, [], L, L).
update_ASP(PId, AId, [PId - X - Y|T], SoFarL, UpdatedL) :- % Append information of assignment activity to final ASP
    activity(AId, act(S, E)),
    NewDuration is Y + E - S,
    update_ASP(PId, AId, T, [PId - [AId|X] - NewDuration|SoFarL], UpdatedL).
update_ASP(PId, AId, [PId2 - X - Y|T], SoFarL, UpdatedL) :-
    PId =\= PId2,
    update_ASP(PId, AId, T, [PId2 - X - Y|SoFarL], UpdatedL).

fill_activities(0, _, []).
fill_activities(N, [AId|RestOfActivities], [AId|RestAIds]) :-
    NewN is N - 1,
    fill_activities(NewN, RestOfActivities, RestAIds).

% include the knowledge base
:- ['load.pro'].

% 3.1 
distance([], [], TailResult):- TailResult = 0.
distance([Head|Tail], [Head2|TaiList2], Result) :-
    distance(Tail, TaiList2, TailResult),

    %Temp = Result;
    %Result is TailResult + Temp,
    (\+(Head=(-1)), Temp is ((Head-Head2)*(Head-Head2)); Temp is 0),
    Result  is  Temp+TailResult.


glanian_distance(Name1, Name2, Pistance) :-
expects(Name1, _, List),
glanian(Name2, _, List2),
distance(List, List2, Temp),
Pistance is sqrt(Temp).
%0.945, -1, 0.186, 0.151, -1, -1, 0.483, 0.343, 0.351, 0.960
%0.936, 0.425, 0.550, 0.613, 0.063, 0.334, 0.834, 0.242, 0.970, 0.172


% 3.2 
weighted_glanian_distance(Name1, Name2, Distance) :-
    expects(Name1, _, List),
    glanian(Name2, _, List2),
    weight(Name1,List3),
    wdistance(List, List2, List3, Temp),
    Distance is sqrt(Temp).


wdistance([], [], [], TailResult):- TailResult = 0.
wdistance([Head|Tail], [Head2|TaiList2], [Headw|Tailw], Result) :-
    wdistance(Tail, TaiList2, Tailw, TailResult),

    %Temp = Result;
    %Result is TailResult + Temp,
    ((\+(Head=(-1)), \+(Headw=(-1))), Temp is (Headw*(Head-Head2)*(Head-Head2)); Temp is 0),
    Result  is  Temp+TailResult.


% 3.3 
find_possible_cities(Name, CityList) :-
    city(X, List, _),
    init(Name, List, Result),
    (Result = true) -> Temp = X,
    likes(Name, _, Y),
    [Temp|Y] = CityList.
    

init(Name, [], false).
init(Name, [Head|Tail], Result):-
    (Head = Name) -> Result = true, !;
    init(Name, Tail, Result).


% 3.4
 merge_possible_cities(Name1, Name2, MergedCities):-
    find_possible_cities(Name1, List1),
    find_possible_cities(Name2, List2),
    concatenate(List1, List2, MergedCities).
    

concatenate(List1, List1, List1).
concatenate([], List, List).
concatenate([H1|T1], List2, [H1|TailResult]) :-
    concatenate(T1, List2, TailResult).

find_mutual_activities(Name1, Name2, MutualActivities):-
    likes(Name1, X, _),
    likes(Name2, Y, _),
    mutual(X,Y, MutualActivities).



mutual([], _, []).
mutual([H|T], List2, [H|List1]):-
     member(H, List2), !,
mutual(T, List2, List1).
mutual([_|T], List2, List1):- 
     mutual(T, List2, List1).


% 3.6 
find_possible_targets(Name, Distances, TargetList):-
    findall(Y, glanian(Y, T, _), Names),
    find_possible_target(Name, Pistances, KargetList, Names),
    sort(Pistances, Listances),
    divide_dashed_list(Listances, Distances, TargetList).
    %sort(TargetList, KargetList).


find_possible_target(Name, [], [], []).
find_possible_target(Name, Distances, TargetList, [HeadNames|TailNames]) :-
    Distances = [HeadD|TailD],
    TargetList = [HeadT|TargetT],
    expects(Name, X, _),
    %findall(Y, glanian(Y, T, _), Names),
    glanian(HeadNames, T, _),
    init(T, X, Result),
    ((Result = true) -> glanian_distance(Name, HeadNames, Distance), find_possible_target(Name, TailD, TargetT, TailNames), HeadD = Distance-HeadNames);
    find_possible_target(Name, Distances, TargetList, TailNames).
    

divide_dashed_list([], [], []).
divide_dashed_list([Head|Tail], [HeadFirst|TailFirst], [HeadSecond|TailSecond]) :-
    HeadFirst-HeadSecond = Head,
    divide_dashed_list(Tail, TailFirst, TailSecond).
    


    

    

% 3.7
 find_weighted_targets(Name, Distances, TargetList):-
    findall(Y, glanian(Y, T, _), Names),
    find_wpossible_target(Name, Pistances, KargetList, Names),
    sort(Pistances, Listances),
    divide_dashed_list(Listances, Distances, TargetList).
    %sort(TargetList, KargetList).


find_wpossible_target(Name, [], [], []).
find_wpossible_target(Name, Distances, TargetList, [HeadNames|TailNames]) :-
    Distances = [HeadD|TailD],
    TargetList = [HeadT|TargetT],
    expects(Name, X, _),
    %findall(Y, glanian(Y, T, _), Names),
    glanian(HeadNames, T, _),
    init(T, X, Result),
    ((Result = true) -> weighted_glanian_distance(Name, HeadNames, Distance), find_wpossible_target(Name, TailD, TargetT, TailNames), HeadD = Distance-HeadNames);
    find_wpossible_target(Name, Distances, TargetList, TailNames).
    

% 3.8 find_my_best_target(Name, Distances, Activities, Cities, Targets) 20 points

% 3.9 find_my_best_match(Name, Distances, Activities, Cities, Targets) 25 points

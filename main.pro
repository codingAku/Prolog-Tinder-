%ecenur sezer
%2018400183
%compiling: yes
%complete: yes

% include the knowledge base
:- ['load.pro'].

% Most of the comments are at below, in helper methods part.
% I did not comment the predicate functions since they are already explained in description. There are only "%3.x" marks indicating which predicate the function is.

% 3.1 
glanian_distance(Name1, Name2, Distance) :-
    expects(Name1, _, List),
    glanian(Name2, _, List2),
    distances(List, List2, Temp),
    Distance is sqrt(Temp),!.

%Helper recursive method of glanian_distance. Takes list of features of Name2 and list of expects of Name1.
distances([], [], TailResult):- TailResult = 0.
distances([Head|Tail], [Head2|TaiList2], Result) :-
    distances(Tail, TaiList2, TailResult),

    (\+(Head=(-1)), Temp is ((Head-Head2)*(Head-Head2)); Temp is 0),
    Result  is  Temp+TailResult.


% 3.2 
weighted_glanian_distance(Name1, Name2, Distance) :-
    expects(Name1, _, List),
    glanian(Name2, _, List2),
    weight(Name1,List3),
    wdistance(List, List2, List3, Temp),
    Distance is sqrt(Temp),!.

%Helper recursive method of weighted_glanian_distance. Takes list of features of Name2 and list of expects of Name1 and weigths of Name1.
wdistance([], [], [], TailResult):- TailResult = 0.
wdistance([Head|Tail], [Head2|TaiList2], [Headw|Tailw], Result) :-
    wdistance(Tail, TaiList2, Tailw, TailResult),

    ((\+(Head=(-1)), \+(Headw=(-1))), Temp is (Headw*(Head-Head2)*(Head-Head2)); Temp is 0),
    Result  is  Temp+TailResult.


% 3.3 
find_possible_cities(Name, CityList) :-
    city(X, List, _),
    (member(Name, List)) -> Temp = X,
    likes(Name, _, Y),
    [Temp|Y] = CityList.
    

% 3.4
 merge_possible_cities(Name1, Name2, MergedCities):-
    find_possible_cities(Name1, List1),
    find_possible_cities(Name2, List2),
    concatenate(List1, List2, MergedCities).
    
%3.5
find_mutual_activities(Name1, Name2, MutualActivities):-
    likes(Name1, X, _),
    likes(Name2, Y, _),
    mutual(X,Y, MutualActivities).


% 3.6 
find_possible_targets(Name, Distances, TargetList):-
    findall(Y, glanian(Y, T, _), Names),
    find_possible_target(Name, Pistances, KargetList, Names),
    sort(Pistances, Listances),
    divide_dashed_list(Listances, Distances, TargetList),!.

%Helper recursive method of find_possible_targets. Takes Name, list of names of glanians as a parameter,
%returns Distances and possible targets as a list.
find_possible_target(Name, [], [], []).
find_possible_target(Name, Distances, TargetList, [HeadNames|TailNames]) :-
    Distances = [HeadD|TailD],
    TargetList = [HeadT|TargetT],
    expects(Name, X, _),
    glanian(HeadNames, T, _),
    ((member(T,X)) -> glanian_distance(Name, HeadNames, Distance), find_possible_target(Name, TailD, TargetT, TailNames), HeadD = Distance-HeadNames);
    find_possible_target(Name, Distances, TargetList, TailNames).


% 3.7
find_weighted_targets(Name, Distances, TargetList):-
    findall(Y, glanian(Y, T, _), Names),
    find_wpossible_target(Name, Pistances, KargetList, Names),
    sort(Pistances, Listances),
    divide_dashed_list(Listances, Distances, TargetList),!.


%helper recursive function of find_weighted_targets. Takes Name, list of names of glanians and weights of Name as a parameter,
%returns Distances and possible targets as a list.
find_wpossible_target(Name, [], [], []).
find_wpossible_target(Name, Distances, TargetList, [HeadNames|TailNames]) :-
    Distances = [HeadD|TailD],
    TargetList = [HeadT|TargetT],
    expects(Name, X, _),
    glanian(HeadNames, T, _),
    ((member(T,X)) -> weighted_glanian_distance(Name, HeadNames, Distance), find_wpossible_target(Name, TailD, TargetT, TailNames), HeadD = Distance-HeadNames);
    find_wpossible_target(Name, Distances, TargetList, TailNames).


% 3.8
find_my_best_target(Name, Distances, Activities, Cities, Targets) :-
    find_compatible_target(Name, TargetList),
    findall(D-A-C-T, bismillah(Name, D, A, C, T, TargetList), List), 
    sort(List, MainList),
    listSplitter(MainList, Distances, Activities, Cities, Targets).




% 3.9 
find_my_best_match(Name, Distances, Activities, Cities, Targets):-
    find_compatible_match(Name, TargetList),
    findall(D-A-C-T, bismillah2(Name, D, A, C, T, TargetList), List), 
    sort(List, MainList),
    listSplitter(MainList, Distances, Activities, Cities, Targets).













%helper methods



%this method finds the necessary list of targets who does not have an old relation with Name in the database for the find_my_best_match function. 
%It also calculates the weighted distances of Name-Target. 
%Creates a list with dashed elements, Target-Distance.
find_compatible_match(Name, List):-
    findall(Target-D, (glanian(Target, _,_),
    not(old_relation([Name, Target])), not(old_relation([Target,Name])),
    weighted_glanian_distance(Name, Target, B), weighted_glanian_distance(Target, Name, A), D is (A+B)/2), List).

%this method finds the Distance, Activity, City, Target, pairs that meet the requirements of the find_my_best_match function. Almost all of the predicates are checked within.
% It also takes the list of compatible targets as a parameter.   
bismillah2(Name, Distance, Activity, City, Target, TargetDList):-
    
    member(X, TargetDList),
    X = Target-Distance,
    glanian(Target, G, _),
    glanian(Name, G1, _),
    expects(Name, GL, _),
    expects(Target, GL1, _),
    member(G, GL),
    member(G1, GL1),
    features(Name, Target),
    features(Target, Name),
    conflict(Name, Target),
    conflict(Target, Name),
    merge_possible_cities(Name, Target, CityList),
    member(City, CityList),
    isCompatible(Name, City, Activity),
    isCompatible(Target, City, Activity).

%this method finds the list of targets in the database who does not have old relations with Name for the find_my_best_target function.
% It also calculates the weighted distances of Name-Target. 
%Creates a list with dashed elements, Target-Distance.
find_compatible_target(Name, List):-
    findall(Target-D, (glanian(Target, _,_),
    not(old_relation([Name, Target])), not(old_relation([Target,Name])),
    weighted_glanian_distance(Name, Target, D)), List).

%this method finds the Distance, Activity, City, Target, pairs that meet the requirements of find_my_best_target function. Almost all of the predicates are checked within.
% It also takes the list of compatible targets as a parameter.   
bismillah(Name, Distance, Activity, City, Target, TargetDList):-
    
    member(X, TargetDList),
    X = Target-Distance,
    %member(Target, TargetList),
    glanian(Target, G, _),
    expects(Name, GL, _),
    member(G, GL),
    features(Name, Target),
    conflict(Name, Target),
    merge_possible_cities(Name, Target, CityList),
    member(City, CityList),
    isCompatible(Name, City, Activity).

%this method finds City, Activity pairs that Name (is a habitant of City, or likes City) and Activity is in City, 
%and Activity is not in DislikedActivities of Name) or (City is not in DislikedCities of Name, and Activity is in City, and Activity is in LikedActivities)
isCompatible(Name, City, Activity):-
    find_possible_cities(Name, Cities),
    dislikes(Name, DActivites, DCities, _),
    likes(Name, LActivities, _),
    city(City, _, Activities),
    ((member(City, Cities), member(Activity, Activities), 
    not(member(Activity, DActivites)));
    (member(Activity, LActivities), member(Activity, Activities), not(member(City, DCities))) ).

%this method compares disliked activities of Name and likes activities of Target and returns true if there are no more than 2 common elements.
conflict(Name, Target):-
    likes(Target, Likes, _),
    dislikes(Name, Dislikes, _,_),
    mutual(Likes, Dislikes, Conflict),
    length(Conflict, Int),
    (Int < 3).


%this method checks if the Target's features are in tolerance limits of Name.
features(Name, Target):-
    
    glanian(Target, _, Features),
    dislikes(Name, _, _, Limits),
    ohgod(Features,Limits).

%This method takes two lists containing features of Target and Limits of Name. 
%Divides the lists and sends an individual feature of Target and limits list of that feature to a method to compare.
%All features are checked like this. Returns true if target's features is in tolerance of Name.
ohgod([],[]).
ohgod([H|T],[H1|T1]):-
    ohgod(T,T1),
    between(H, H1).

%Compares if Num1 is in limits of a list with two integers.
between(Num1, []).
between(Num1, [H|[H1|T1]]):-
    (Num1 >= H),
    (Num1 =< H1).

%takes a four-dashed list and divides them to four different list.
listSplitter(List, R1, R2, R3, R4):-
    divide_dashed_list(List, Temp1, R4),
    divide_dashed_list(Temp1, Temp2, R3),
    divide_dashed_list(Temp2, R1, R2).

%concatenates two list to a new one.
concatenate(List1, List1, List1).
concatenate([], List, List).
concatenate([H1|T1], List2, [H1|TailResult]) :-
    concatenate(T1, List2, TailResult).
%Takes two lists as parameter and a third one to keep the mutual elements in the first two list.
mutual([], _, []).
mutual([H|T], List2, [H|List1]):-
    member(H, List2), !,
    mutual(T, List2, List1).
mutual([_|T], List2, List1):- 
    mutual(T, List2, List1).

%divides a two dashed list into two different list.
divide_dashed_list([], [], []).
divide_dashed_list([Head|Tail], [HeadFirst|TailFirst], [HeadSecond|TailSecond]) :-
    HeadFirst-HeadSecond = Head,
    divide_dashed_list(Tail, TailFirst, TailSecond).%ecenur sezer
%2018400183
%compiling: yes
%complete: yes

% include the knowledge base
:- ['load.pro'].

% Most of the comments are at below, in helper methods part.
% I did not comment the predicate functions since they are already explained in description. There are only "%3.x" marks indicating which predicate the function is.

% 3.1 
glanian_distance(Name1, Name2, Distance) :-
    expects(Name1, _, List),
    glanian(Name2, _, List2),
    distances(List, List2, Temp),
    Distance is sqrt(Temp),!.

%Helper recursive method of glanian_distance. Takes list of features of Name2 and list of expects of Name1.
distances([], [], TailResult):- TailResult = 0.
distances([Head|Tail], [Head2|TaiList2], Result) :-
    distances(Tail, TaiList2, TailResult),

    (\+(Head=(-1)), Temp is ((Head-Head2)*(Head-Head2)); Temp is 0),
    Result  is  Temp+TailResult.


% 3.2 
weighted_glanian_distance(Name1, Name2, Distance) :-
    expects(Name1, _, List),
    glanian(Name2, _, List2),
    weight(Name1,List3),
    wdistance(List, List2, List3, Temp),
    Distance is sqrt(Temp),!.

%Helper recursive method of weighted_glanian_distance. Takes list of features of Name2 and list of expects of Name1 and weigths of Name1.
wdistance([], [], [], TailResult):- TailResult = 0.
wdistance([Head|Tail], [Head2|TaiList2], [Headw|Tailw], Result) :-
    wdistance(Tail, TaiList2, Tailw, TailResult),

    ((\+(Head=(-1)), \+(Headw=(-1))), Temp is (Headw*(Head-Head2)*(Head-Head2)); Temp is 0),
    Result  is  Temp+TailResult.


% 3.3 
find_possible_cities(Name, CityList) :-
    city(X, List, _),
    (member(Name, List)) -> Temp = X,
    likes(Name, _, Y),
    [Temp|Y] = CityList.
    

% 3.4
 merge_possible_cities(Name1, Name2, MergedCities):-
    find_possible_cities(Name1, List1),
    find_possible_cities(Name2, List2),
    concatenate(List1, List2, MergedCities).
    
%3.5
find_mutual_activities(Name1, Name2, MutualActivities):-
    likes(Name1, X, _),
    likes(Name2, Y, _),
    mutual(X,Y, MutualActivities).


% 3.6 
find_possible_targets(Name, Distances, TargetList):-
    findall(Y, glanian(Y, T, _), Names),
    find_possible_target(Name, Pistances, KargetList, Names),
    sort(Pistances, Listances),
    divide_dashed_list(Listances, Distances, TargetList),!.

%Helper recursive method of find_possible_targets. Takes Name, list of names of glanians as a parameter,
%returns Distances and possible targets as a list.
find_possible_target(Name, [], [], []).
find_possible_target(Name, Distances, TargetList, [HeadNames|TailNames]) :-
    Distances = [HeadD|TailD],
    TargetList = [HeadT|TargetT],
    expects(Name, X, _),
    glanian(HeadNames, T, _),
    ((member(T,X)) -> glanian_distance(Name, HeadNames, Distance), find_possible_target(Name, TailD, TargetT, TailNames), HeadD = Distance-HeadNames);
    find_possible_target(Name, Distances, TargetList, TailNames).


% 3.7
find_weighted_targets(Name, Distances, TargetList):-
    findall(Y, glanian(Y, T, _), Names),
    find_wpossible_target(Name, Pistances, KargetList, Names),
    sort(Pistances, Listances),
    divide_dashed_list(Listances, Distances, TargetList),!.


%helper recursive function of find_weighted_targets. Takes Name, list of names of glanians and weights of Name as a parameter,
%returns Distances and possible targets as a list.
find_wpossible_target(Name, [], [], []).
find_wpossible_target(Name, Distances, TargetList, [HeadNames|TailNames]) :-
    Distances = [HeadD|TailD],
    TargetList = [HeadT|TargetT],
    expects(Name, X, _),
    glanian(HeadNames, T, _),
    ((member(T,X)) -> weighted_glanian_distance(Name, HeadNames, Distance), find_wpossible_target(Name, TailD, TargetT, TailNames), HeadD = Distance-HeadNames);
    find_wpossible_target(Name, Distances, TargetList, TailNames).


% 3.8
find_my_best_target(Name, Distances, Activities, Cities, Targets) :-
    find_compatible_target(Name, TargetList),
    findall(D-A-C-T, bismillah(Name, D, A, C, T, TargetList), List), 
    sort(List, MainList),
    listSplitter(MainList, Distances, Activities, Cities, Targets).




% 3.9 
find_my_best_match(Name, Distances, Activities, Cities, Targets):-
    find_compatible_match(Name, TargetList),
    findall(D-A-C-T, bismillah2(Name, D, A, C, T, TargetList), List), 
    sort(List, MainList),
    listSplitter(MainList, Distances, Activities, Cities, Targets).













%helper methods



%this method finds the necessary list of targets who does not have an old relation with Name in the database for the find_my_best_match function. 
%It also calculates the weighted distances of Name-Target. 
%Creates a list with dashed elements, Target-Distance.
find_compatible_match(Name, List):-
    findall(Target-D, (glanian(Target, _,_),
    not(old_relation([Name, Target])), not(old_relation([Target,Name])),
    weighted_glanian_distance(Name, Target, B), weighted_glanian_distance(Target, Name, A), D is (A+B)/2), List).

%this method finds the Distance, Activity, City, Target, pairs that meet the requirements of the find_my_best_match function. Almost all of the predicates are checked within.
% It also takes the list of compatible targets as a parameter.   
bismillah2(Name, Distance, Activity, City, Target, TargetDList):-
    
    member(X, TargetDList),
    X = Target-Distance,
    glanian(Target, G, _),
    glanian(Name, G1, _),
    expects(Name, GL, _),
    expects(Target, GL1, _),
    member(G, GL),
    member(G1, GL1),
    features(Name, Target),
    features(Target, Name),
    conflict(Name, Target),
    conflict(Target, Name),
    merge_possible_cities(Name, Target, CityList),
    member(City, CityList),
    isCompatible(Name, City, Activity),
    isCompatible(Target, City, Activity).

%this method finds the list of targets in the database who does not have old relations with Name for the find_my_best_target function.
% It also calculates the weighted distances of Name-Target. 
%Creates a list with dashed elements, Target-Distance.
find_compatible_target(Name, List):-
    findall(Target-D, (glanian(Target, _,_),
    not(old_relation([Name, Target])), not(old_relation([Target,Name])),
    weighted_glanian_distance(Name, Target, D)), List).

%this method finds the Distance, Activity, City, Target, pairs that meet the requirements of find_my_best_target function. Almost all of the predicates are checked within.
% It also takes the list of compatible targets as a parameter.   
bismillah(Name, Distance, Activity, City, Target, TargetDList):-
    
    member(X, TargetDList),
    X = Target-Distance,
    %member(Target, TargetList),
    glanian(Target, G, _),
    expects(Name, GL, _),
    member(G, GL),
    features(Name, Target),
    conflict(Name, Target),
    merge_possible_cities(Name, Target, CityList),
    member(City, CityList),
    isCompatible(Name, City, Activity).

%this method finds City, Activity pairs that Name (is a habitant of City, or likes City) and Activity is in City, 
%and Activity is not in DislikedActivities of Name) or (City is not in DislikedCities of Name, and Activity is in City, and Activity is in LikedActivities)
isCompatible(Name, City, Activity):-
    find_possible_cities(Name, Cities),
    dislikes(Name, DActivites, DCities, _),
    likes(Name, LActivities, _),
    city(City, _, Activities),
    ((member(City, Cities), member(Activity, Activities), 
    not(member(Activity, DActivites)));
    (member(Activity, LActivities), member(Activity, Activities), not(member(City, DCities))) ).

%this method compares disliked activities of Name and likes activities of Target and returns true if there are no more than 2 common elements.
conflict(Name, Target):-
    likes(Target, Likes, _),
    dislikes(Name, Dislikes, _,_),
    mutual(Likes, Dislikes, Conflict),
    length(Conflict, Int),
    (Int < 3).


%this method checks if the Target's features are in tolerance limits of Name.
features(Name, Target):-
    
    glanian(Target, _, Features),
    dislikes(Name, _, _, Limits),
    ohgod(Features,Limits).

%This method takes two lists containing features of Target and Limits of Name. 
%Divides the lists and sends an individual feature of Target and limits list of that feature to a method to compare.
%All features are checked like this. Returns true if target's features is in tolerance of Name.
ohgod([],[]).
ohgod([H|T],[H1|T1]):-
    ohgod(T,T1),
    between(H, H1).

%Compares if Num1 is in limits of a list with two integers.
between(Num1, []).
between(Num1, [H|[H1|T1]]):-
    (Num1 >= H),
    (Num1 =< H1).

%takes a four-dashed list and divides them to four different list.
listSplitter(List, R1, R2, R3, R4):-
    divide_dashed_list(List, Temp1, R4),
    divide_dashed_list(Temp1, Temp2, R3),
    divide_dashed_list(Temp2, R1, R2).

%concatenates two list to a new one.
concatenate(List1, List1, List1).
concatenate([], List, List).
concatenate([H1|T1], List2, [H1|TailResult]) :-
    concatenate(T1, List2, TailResult).
%Takes two lists as parameter and a third one to keep the mutual elements in the first two list.
mutual([], _, []).
mutual([H|T], List2, [H|List1]):-
    member(H, List2), !,
    mutual(T, List2, List1).
mutual([_|T], List2, List1):- 
    mutual(T, List2, List1).

%divides a two dashed list into two different list.
divide_dashed_list([], [], []).
divide_dashed_list([Head|Tail], [HeadFirst|TailFirst], [HeadSecond|TailSecond]) :-
    HeadFirst-HeadSecond = Head,
    divide_dashed_list(Tail, TailFirst, TailSecond).

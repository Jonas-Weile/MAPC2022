

obstacle(Xr, Yr) :-
	thing(Xr, Yr, obstacle, _).
			
 % Convert name to 'random' seed								    
nameToSeed(Name, Seed) :- 									    
 	sub_string(Name, B, L, _, "agentGOAL-DTU"),
 	BL is B+L,
 	sub_string(Name, BL, _, 0, Rest),
 	number_string(SeedX, Rest),
 	Seed is ((SeedX-1)*3+5) mod 24.


% Take the number from the name
nameToNumber(Name, Number) :- 									    
 	sub_string(Name, _, Length, _, "agentGOAL-DTU"),
 	sub_string(Name, Length, _, 0, Number_str),
 	atom_number(Number_str, Number).
 	
 	
 nameToChannel(Name, Channel) :-
 	nameToNumber(Name, Number),
 	string_concat("Channel", Number, Channel).

 	
taskToChannel(TaskMaster, TaskName, ChannelName) :-
	string_concat(TaskMaster, "_", FirstHalf),
 	string_concat(TaskName, "_Channel", SecondHalf),
 	string_concat(FirstHalf, SecondHalf, ChannelName).
 	

% Absolute value of a number
abs(X1, X2) :- X1 < 0, !, X2 is -X1.
abs(X, X).


% Approximation to euclidean distance
distMan(X1, Y1, X2, Y2, Res) :- X is X2-X1, abs(X, XAbs), Y is Y2-Y1, abs(Y, YAbs), Res is (XAbs+YAbs).
distEuc(X1, Y1, X2, Y2, D) :- D is sqrt((X2-X1)^2 + (Y2-Y1)^2).

% Reverse sort a list
reverseSort(List, ListSortedRev) :- sort(List, ListSorted), rev(ListSorted, ListSortedRev).
rev([], []).
rev([H|T], R) :- rev(T, Rt), append(Rt, [H], R). 


% List sum
listSum([], 0, 0).
listSum([H|T], Sum, Length) :-
    listSum(T, TailSum, TailLength),
    Sum is H + TailSum,
    Length is 1 + TailLength.



% Generate 'randomly' numerated lists of directions
generateEnumDirList(P, V, InitialSeed) :-
	findall(Px, permutation([n, e, s, w], Px), PL),
	generateEnumDirList(PL, InitialSeed, P, V).
generateEnumDirList([P|_], V, P, V).
generateEnumDirList([_|PL], C, P, V) :-
	Cx is (C + 1),
	Cxx is Cx mod 24,
	generateEnumDirList(PL, Cxx, P, V). 

	
% Greatest common divisor
findGcdFromList([H|T], GCD) :-
	foldl(gcd_helper, T, H, GCD).
	
gcd_helper(X, Y, Z) :-
    X < 0, !,
    gcd_helper(-X, Y, Z).
gcd_helper(X, Y, Z) :-
    Y < 0, !,
    gcd_helper(X, -Y, Z).
gcd_helper(X, 0, X) :- X > 0.
gcd_helper(0, Y, Y) :- Y > 0.
gcd_helper(X, Y, Z) :-
    X > Y, Y > 0,
    X1 is X - Y,
    gcd_helper(Y, X1, Z).
gcd_helper(X, Y, Z) :-
    X =< Y, X > 0,
    Y1 is Y - X,
    gcd_helper(X, Y1, Z).
    
    
collectListsToSets((L1, L2, L3), (L4, L5, L6), (R1, R2, R3)) :- 
	sort(L1, L1_sorted), sort(L2, L2_sorted), sort(L3, L3_sorted),
	sort(L4, L4_sorted), sort(L5, L5_sorted), sort(L6, L6_sorted),
	
	ord_union(L1_sorted, L4_sorted, R1),
	ord_union(L2_sorted, L5_sorted, R2),
	ord_union(L3_sorted, L6_sorted, R3).
	
	
% Count occurences of an element in a list
% count(+List, +Elemt, ?Count).
count([],_,0).
count([X|T],X,Y):- !, count(T,X,Z), Y is 1+Z.
count([_|T],X,Z):- count(T,X,Z).



addOffset(OffsetX, OffsetY, Input, Output) :-
	Input = dispenser(Xd, Yd, Type) -> 
		(offsetAndTranslateToMyOrigin(OffsetX, OffsetY, Xd, Yd, XFromMyOrigin, YFromMyOrigin),
		 Output = dispenser(XFromMyOrigin, YFromMyOrigin, Type)) 
	;
	(Input = goalCell(Xgc, Ygc),
		(offsetAndTranslateToMyOrigin(OffsetX, OffsetY, Xgc, Ygc, XFromMyOrigin, YFromMyOrigin),
		 Output = goalCell(XFromMyOrigin, YFromMyOrigin)))
	;
	(Input = roleCell(Xrc, Yrc),
		(offsetAndTranslateToMyOrigin(OffsetX, OffsetY, Xrc, Yrc, XFromMyOrigin, YFromMyOrigin),
		 Output = roleCell(XFromMyOrigin, YFromMyOrigin))).


addOffset_list(_, _, [], []).
addOffset_list(OffsetX, OffsetY, InputList, OutputList) :-
	findall(Output,
		(
			member(Input, InputList),
			addOffset(OffsetX, OffsetY, Input, Output)
		),
		OutputList).

   

offsetAndTranslateToMyOrigin(OffsetX, OffsetY, X, Y, XFromMyOrigin, YFromMyOrigin) :-
	X_helper is X + OffsetX,
	Y_helper is Y + OffsetY,
	translateCoordinatesToMyOrigin(X_helper, Y_helper, XFromMyOrigin, YFromMyOrigin).

% Add offset to an agent
translateToMyOrigin_Agent(OffsetX, OffsetY, (Agent, AgentX, AgentY), ResultingAgent) :-
	X is AgentX + OffsetX,
	Y is AgentY + OffsetY,
	translateCoordinatesToMyOrigin(X, Y, XFromMyOrigin, YFromMyOrigin),
	ResultingAgent = (Agent, XFromMyOrigin, YFromMyOrigin).
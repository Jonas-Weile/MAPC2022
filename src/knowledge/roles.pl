canAttach :-
 	myRole(MyRole), canAttach(MyRole).
 	
 canAttach(Role) :-
 	role(Role, Vision, Actions, Speeds, ClearChance, ClearMaxDistance),
 	member(attach, Actions).
 	
 			
maxClearDistance(MaxClearDistance) :-
	myRole(MyRole),
	role(MyRole, Vision, Actions, Speeds, ClearChance, MaxClearDistance).
 
visionForRole(Role, Vision) :-
	role(Role, Vision, _, _, _, _).
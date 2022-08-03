 canAttach(Role) :-
 	role(Role, Vision, Actions, Speeds, ClearChance, ClearMaxDistance),
 	member(attach, Actions).
 	
visionForRole(Role, Vision) :-
	role(Role, Vision, _, _, _, _).
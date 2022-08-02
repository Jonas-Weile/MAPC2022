 canAttach(Role) :-
 	role(Role, Vision, Actions, Speeds, ClearChance, ClearMaxDistance),
 	member(attach, Actions).
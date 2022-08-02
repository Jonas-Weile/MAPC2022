include(89898)

:- dynamic
 
 	%%%%%% COMMON %%%%%%
	name/1, team/1, teamSize/1, steps/1, vision/1, clearEnergyCost/1, maxEnergy/1, 
	clearing/2, clearSteps/1, clearStepsCounter/1, completedClearAction/1, role/6,
	
	% Environment percepts
	step/1, score/1, lastAction/1, lastActionResult/1, lastActionParams/1,
	energy/1, deactivated/1, task/4, attached/2, obstacle/2, % thiings
	thing/4, accepted/1, role/1, goalZone/2, roleZone/2, norm/6, violation/1,
	
	% Things
	taskboard/2, dispenser/3,
	
	% Internal percepts
	goalCell/2, roleCell/2.
   
  


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

%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% a_star.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%% 
astarNoClear(GoalXr,GoalYr,Path, Actions) :- 
    empty_heap(Frontier),
    getAttachments(Attachments),
    energy(Level),
    expandFrontierNoClear(0, 0, 0, 0, Level, [], [], Attachments, [], GoalXr, GoalYr, Frontier, [], ExpandedFrontier),
    astarRecursiveNoClear(GoalXr, GoalYr, ExpandedFrontier, [state((0,0), Attachments, [])], [(FinalX, FinalY)|RevPath], RevActions),!,
    distanceBetweenPoints_Euclidian(0, 0, GoalXr, GoalYr, StartDist),
    distanceBetweenPoints_Euclidian(FinalX, FinalY, GoalXr, GoalYr, ResultDist),
    ResultDist < StartDist,
    reverse([(FinalX, FinalY)|RevPath], Path_relative),
    maplist(relativeToAbsolutePositionOfPoints, Path_relative, Path),
    reverse(RevActions, Actions).

astarClear(GoalXr, GoalYr, Path, Actions) :- 
    empty_heap(Frontier),
    getAttachments(Attachments),
    energy(Level),
    validClearActions(0,0, 0, 0, Level, [], [], Attachments, [], GoalXr, GoalYr, [], Frontier, ExpandedFrontier),
    astarRecursiveClear(GoalXr, GoalYr, ExpandedFrontier, [state((0,0), Attachments, [])], [(FinalX, FinalY)|RevPath], RevActions),!,
    distanceBetweenPoints_Euclidian(0, 0, GoalXr, GoalYr, StartDist),
    distanceBetweenPoints_Euclidian(FinalX, FinalY, GoalXr, GoalYr, ResultDist),
    ResultDist < StartDist,
    reverse([(FinalX, FinalY)|RevPath], Path_relative),
    maplist(relativeToAbsolutePositionOfPoints, Path_relative, Path),
    reverse(RevActions, Actions).
   
   
   
    
astarRecursiveNoClear(GoalX,GoalY,Frontier, _, [(X,Y)| Path], CompleteActions) :-
    get_from_heap(Frontier,_,state((X,Y), _, _, _, Path, CompleteActions, _, _), _),
	(
		(X = GoalX, Y = GoalY)
	;
		distMan(0, 0, X, Y, 5)
	),	
    !.

astarRecursiveClear(GoalX,GoalY,Frontier, _, [(X,Y)| Path], CompleteActions) :-
    get_from_heap(Frontier,_,state((X,Y), _, _, _, Path, CompleteActions, _, _), _),
	(
		(X = GoalX, Y = GoalY)
	;
		distMan(0, 0, X, Y, 5)
	),	
    !.
   
	
astarRecursiveNoClear(GoalX,GoalY,Frontier, ExpandedStates, CompletePath, CompleteActions) :-
    get_from_heap(Frontier,_,state((X,Y), ParentStepCost, ParentRotationLimit, ParentEnergy, ParentPath, ParentActions, ParentAttachments, ParentClearedCells), Frontier1),
    expandFrontierNoClear(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, ParentPath, ParentActions,ParentAttachments, ParentClearedCells, GoalX,GoalY,Frontier1, ExpandedStates, ExpandedFrontier),
    astarRecursiveNoClear(GoalX,GoalY,ExpandedFrontier, [state((X,Y), ParentAttachments, ParentClearedCells)|ExpandedStates], CompletePath, CompleteActions).

astarRecursiveClear(GoalX,GoalY,Frontier, ExpandedStates, CompletePath, CompleteActions) :-
    get_from_heap(Frontier,_,state((X,Y), ParentStepCost, ParentRotationLimit, ParentEnergy, ParentPath, ParentActions, ParentAttachments, ParentClearedCells), Frontier1),
    expandFrontierClear(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, ParentPath, ParentActions,ParentAttachments, ParentClearedCells, GoalX,GoalY,Frontier1, ExpandedStates, ExpandedFrontier),
    astarRecursiveClear(GoalX,GoalY,ExpandedFrontier, [state((X,Y), ParentAttachments, ParentClearedCells)|ExpandedStates], CompletePath, CompleteActions).


expandFrontierNoClear(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, ParentPath, ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, Frontier, ExpandedStates, ExpandedFrontier):-
    validMoveActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, Frontier, MoveFrontier),!,
    validRotateActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, MoveFrontier, ExpandedFrontier),!.
   
expandFrontierClear(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, ParentPath, ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, Frontier, ExpandedStates, ExpandedFrontier):-
    validMoveActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, Frontier, MoveFrontier),!,
    validRotateActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, MoveFrontier, RotateFrontier),!,
    validClearActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, RotateFrontier, ExpandedFrontier),!.
    
    
validMoveActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, Path, ParentActions, Attachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, Frontier, ExpandedFrontier):-
    StepCost is ParentStepCost +1,
    updateEnergy(ParentEnergy, UpdatedEnergy),
    findall(Heuristic-state((AdjX,AdjY), StepCost, ParentRotationLimit, UpdatedEnergy, Path, [move(D)|ParentActions], Attachments, ParentClearedCells),
            (
				translate(D, X, Y, AdjX, AdjY),
            	not(astarBlocked(D, X, Y, Attachments, ParentClearedCells)),
                not(member(state((AdjX,AdjY), Attachments, ParentClearedCells),ExpandedStates)), %%% Find less exhaustive solution.
				heuristic(AdjX, AdjY, GoalX, GoalY, StepCost, Heuristic)
			),
            PassableStates),
	insertListInHeap(PassableStates, Frontier, ExpandedFrontier).
	 	

validRotateActions(_,_,_, _, _, _, _, [], _, _, _, _,Frontier,Frontier).
validRotateActions(X,Y,_, 1, _, _, _, _, _, _, _, _,Frontier,Frontier):- 
	not(obstacleClose(X,Y)).
validRotateActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, Path, ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, Frontier, ExpandedFrontier):-
    (ParentRotationLimit = 1 -> RotationLimit = 0; RotationLimit is ParentRotationLimit+1),
    StepCost is ParentStepCost +1,
	heuristic(X, Y, GoalX, GoalY, StepCost, Heuristic),
    updateEnergy(ParentEnergy, UpdatedEnergy),
    findall(Heuristic-state((X,Y), StepCost, RotationLimit, UpdatedEnergy, Path, [rotate(R)|ParentActions], Attachments, ParentClearedCells),
            (member(R,[cw,ccw]),
                not(astarBlockedRotation(X, Y, R, ParentAttachments,ParentClearedCells)), 
            	rotateAttachments(R,ParentAttachments, Attachments),
            	not(member(state((X,Y), Attachments, ParentClearedCells),ExpandedStates))),
            PassableStates),
    insertListInHeap(PassableStates, Frontier, ExpandedFrontier).
    
validClearActions(_,_, _, _, ParentEnergy, _, _, _, _, _, _, _, Frontier, Frontier):-
    not(checkEnergy(ParentEnergy, _)).
validClearActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, Path, ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, Frontier, ExpandedFrontier):-
    clearSteps(ClearSteps),
    team(Team),
    checkEnergy(ParentEnergy, EnergyAfterClear),
    updateEnergy(EnergyAfterClear, UpdatedEnergy),
    StepCost is ParentStepCost + ClearSteps,
    heuristic(X, Y, GoalX, GoalY, StepCost, Heuristic),
    cellsInRange(X, Y, 2, 3, PotentialClearCells), 
    findall(Heuristic-state((X,Y), StepCost, ParentRotationLimit, UpdatedEnergy, Path, [clear(CellX, CellY)|ParentActions], ParentAttachments, AllClearedCells),
            (member((CellX, CellY), PotentialClearCells),
            clearedCells(CellX, CellY, ClearedCells),
            not(( member((ClearedX, ClearedY), ClearedCells), (attached(ClearedX, ClearedY) ; thing(ClearedX, ClearedY, entity, Team)) )),
            append(ClearedCells, ParentClearedCells, AllClearedCells),
            not(member(state((X,Y), ParentAttachments, AllClearedCells),ExpandedStates))
            ),
            PassableStates),
    insertListInHeap(PassableStates, Frontier, ExpandedFrontier).



heuristic(Xr, Yr, GoalXr, GoalYr, StepCost, Heuristic) :-
    distMan(Xr, Yr, GoalXr, GoalYr, Dist),
    Heuristic is Dist + StepCost.



insertListInHeap(List, Heap, ExpandedHeap):-  
	list_to_heap(List, ListHeap),
    	merge_heaps(Heap, ListHeap, ExpandedHeap).


clearedCells(CellX, CellY, ClearedCells):-
	cellsInRange(CellX, CellY, 1, Cells), 
	findall((X, Y),
		(member((X, Y), Cells),
		(
			thing(X, Y, block, _);
			obstacle(X, Y)
		)),
		ClearedCells).

checkEnergy(CurrentEnergy, UpdatedEnergy):-
    clearSteps(ClearSteps),
    clearEnergyCost(ClearCost), 
    TotalEnergyCost = ClearSteps*ClearCost,
    CurrentEnergy >= TotalEnergyCost,
    UpdatedEnergy is CurrentEnergy - TotalEnergyCost.

updateEnergy(CurrentEnergy, UpdatedEnergy):- 
	((maxEnergy(MaxLevel), CurrentEnergy < MaxLevel) -> UpdatedEnergy is CurrentEnergy +1; UpdatedEnergy is CurrentEnergy).

obstacleClose(X,Y):-
	surroundingCells(X,Y, SurroundingCells),
	member((RelativeAdjacentX,RelativeAdjacentY), SurroundingCells), 
	obstacle(RelativeAdjacentX,RelativeAdjacentY).
 
surroundingCells(X,Y,Cells):-
	findall((RelativeAdjacentX,RelativeAdjacentY),
			(between(-1,1,LocalAdjacentX), 
			between(-1,1,LocalAdjacentY),
			not((LocalAdjacentX = 0, LocalAdjacentY = 0)), 
			RelativeAdjacentX is X+LocalAdjacentX, 
			RelativeAdjacentY is Y+LocalAdjacentY),
		Cells).


astarBlockedRotation(MyX, MyY, R, Attachments, ClearedCells) :- 
	member(attachedToMe(X_att, Y_att, _, _),Attachments),
	rotation90(R, X_att, Y_att, Xr, Yr),
	X is MyX + Xr,
	Y is MyY + Yr,
	astarImpassable(X, Y, MyX, MyY),
	not(member((X, Y), ClearedCells)).
	

astarBlocked(D, X, Y, _, ClearedCells) :- 
	translate(D, X, Y, X1, Y1),
	astarImpassable(X1, Y1,X,Y),
        not(member((X1,Y1),ClearedCells)),
	!.
astarBlocked(D, X, Y, Attachments,  ClearedCells) :-
	member(attachedToMe(AttachmentXLocal, AttachmentYLocal, _, _), Attachments),
    	AttachmentXRelative is X + AttachmentXLocal,
    	AttachmentYRelative is Y + AttachmentYLocal,
	translate(D, AttachmentXRelative, AttachmentYRelative, X1,Y1),
	astarImpassable(X1,Y1, X,Y),
    not(member((X1,Y1),ClearedCells)),
	!.
	
astarImpassable(X, Y,_,_) :- obstacle(X, Y).
astarImpassable(X, Y,_,_) :- thing(X, Y, entity, _).
astarImpassable(X, Y, MyX, MyY) :- thing(X, Y, block, BlockType), AttahcmentX is X-MyX, AttahcmentY is Y-MyY, not(attachedToMe(AttahcmentX, AttahcmentY, block, BlockType)).


cellsInRange(X,Y, Range, Cells) :-
    NegRange is 0 - Range,
    findall((CellX,CellY), 
            (
				between(NegRange,Range,X1), 
				between(NegRange,Range, Y1), 
				CellX is X1 + X,
				CellY is Y1 + Y,
				distMan(X, Y,CellX,CellY,D), 
				D =< Range
			 ), 
			Cells).
			
			
cellsInRange(X,Y, MinRange, MaxRange, Cells) :-
    NegMaxRange is 0 - MaxRange,
    findall((CellX,CellY), 
            (
				between(NegMaxRange,MaxRange,X1), 
				between(NegMaxRange,MaxRange, Y1), 
				CellX is X1 + X,
				CellY is Y1 + Y,
				distMan(X, Y,CellX,CellY,D), 
				D =< MaxRange,
				D >= MinRange
			 ), 
			Cells). 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% communication.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%% 
findAdjacentAgents(Agents) :-
	findall((Dist, Xr, Yr), 
		(
			thing(Xr, Yr, entity, Team), 
			team(Team), 
			distEuc(0, 0, Xr, Yr, Dist)
		), 
		AgentsInSight),
	sort(AgentsInSight, AgentsInSightSorted),
	findAdjacentAgents(AgentsInSightSorted, [], [], Agents).
	
findAdjacentAgents([], _, Agents, Agents).
findAdjacentAgents([(Dist1, X1, Y1)|T], NotConnected, Connected, AdjacentAgents) :-
   	((member((X2, Y2), Connected), distEuc(X1, Y1, X2, Y2, Dist2), Dist2 < Dist1);
   	 (member((X3, Y3), NotConnected), distEuc(X1, Y1, X3, Y3, Dist3), Dist3 < Dist1)
  	) ->
		findAdjacentAgents(T, [(X1, Y1)|NotConnected], Connected, AdjacentAgents);
		findAdjacentAgents(T, NotConnected,[(X1, Y1)|Connected], AdjacentAgents).
		
		
		
identifyCommonEnvironmentPercepts(Xr, Yr, Cep) :-
	visionForRole(default, VisionRange),
	findall([Xe, Ye, Type], 
		(  
		   (  
		      thing(Xe, Ye, Type, _);
		      (obstacle(Xe, Ye), Type = obstacle);
		      (goalZone(Xe, Ye), Type = goalZone);
		      (roleZone(Xe, Ye), Type = roleZone)
		   ),
		   (Xe, Ye) \= (Xr, Yr),
		   
		   distMan(Xr, Yr, Xe, Ye, D1), D1 =< VisionRange,
		   distMan(0, 0, Xe, Ye, D2), D2 =< VisionRange		   
		), 
		Cep
	       ).
	       
	       	
getAllConnectionRequestsFromStep(Step, ConnectionRequests, Len) :-
	findall([AgentName, AgentCoordinates, CEPs, Step],
		connectionRequest(AgentName, AgentCoordinates, CEPs, Step),
		ConnectionRequests),
	length(ConnectionRequests, Len).


uniqueConnectionRequests(Requests, UniqueRequests) :-
	findall((Agent, AgentX, AgentY, Xr, Yr, CEP),
		(
		   member([Agent, [AgentX, AgentY], [Xr, Yr, CEP], _], Requests),
		   CEP \= []
		),
		NonEmptyRequests),
	
	findall((Agent, AgentX, AgentY, Xr, Yr, CEP),
		(
		   member((Agent, AgentX, AgentY, Xr, Yr, CEP), NonEmptyRequests),
		   not(( 
		   	member((OtherAgent, _, _, Xr, Yr, OtherCEP), NonEmptyRequests),
		   	OtherAgent \= Agent,
		   	sort(CEP, CEPsorted),
		   	sort(OtherCEP, CEPsorted)
		   ))
		),
		UniqueRequests).


matchingEnvironmentPercepts(Xr, Yr, CEP, Step, MyX, MyY) :-
	X is -Xr, Y is -Yr,
	savedCommonEnvironmentPercepts([MyX, MyY], [X, Y, MyCEP], Step),
	matchEnvironmentPercepts(CEP, MyCEP, X, Y).
	
matchEnvironmentPercepts(CEP1, CEP2, X, Y) :-
	findall([X2, Y2, Type],
		(
			member([X1, Y1, Type], CEP1),
			X2 is X1+X, Y2 is Y1+Y
		),
		CEP_translated),
	sort(CEP2, CEP2_sorted),
	sort(CEP_translated, CEP2_sorted).
	
	
connectedAgentsOrdered(ConnectedAgents_ord) :-
	findall(Agent, agentOffset(Agent, _, _), Agents),
	list_to_ord_set(Agents, Agents_ord),
	name(MyName),
	ord_add_element(Agents_ord, MyName, ConnectedAgents_ord).
		

		




savedConnectionUpdates(SavedUpdates) :-
	findall(Update, connectionUpdate(Update), SavedUpdates).
	
	
findNewConnections(ConnectionUpdateList, NewConnections, MapDimensions) :-
    	name(MyName),
    	foldl(findNC_folder(ConnectionUpdateList), [(MyName, 0, 0)], ([], []), (AllConnections, FoundMapDimensions)),
    
    	findall((AgentName, OffsetX, OffsetY),
    	    	agentOffset(AgentName, OffsetX, OffsetY),
    	    	OldConnections),
    	    
    	ord_del_element(AllConnections, (MyName, 0, 0), AllConnectionsWithoutMe),
    	ord_subtract(AllConnectionsWithoutMe, OldConnections, NewConnections),
	
	findBestMapDimensions(FoundMapDimensions, MapDimensions).
	
	
findNC_folder(_, Agent, Input, Output) :-
	Agent = (AgentName, OffsetX, OffsetY),
	Input = (CheckedConnections, FoundWidhtsAndHeights),
	member((AgentName, OffsetX, OffsetY), CheckedConnections),
	!,
	Output = (CheckedConnections, FoundWidhtsAndHeights).
	
	
findNC_folder(_, Agent, Input, Output) :-
	Agent = (AgentName, OffsetX, OffsetY),
	Input = (CheckedConnections, FoundWidhtsAndHeights),
	
	member((AgentName, OtherOffsetX, OtherOffsetY), CheckedConnections),
	(OtherOffsetX \= OffsetX ; OtherOffsetY \= OffsetY),
	findCircumferencesFromOffsets(OffsetX, OffsetY, OtherOffsetX, OtherOffsetY, L),
	!,
	
	append(L, FoundWidhtsAndHeights, NewFoundWidhtsAndHeights),
	sort(NewFoundWidhtsAndHeights, NewFoundWidhtsAndHeights_Sorted),
	Output = (CheckedConnections, NewFoundWidhtsAndHeights_Sorted).
	

findNC_folder(ConnectionUpdateList, Agent, Input, Output) :-
	Agent = (AgentName, OffsetX, OffsetY), 
	Input = (CheckedConnections, FoundWidhtsAndHeights),
	
	member((AgentName, AgentNewConnections, AgentOldConnections, _, _), ConnectionUpdateList),
	ord_union(AgentNewConnections, AgentOldConnections, AgentConnections),
	
	maplist(translateToMyOrigin_Agent(OffsetX, OffsetY), AgentConnections, AgentConnections_offset),
	 
	translateCoordinatesToMyOrigin(OffsetX, OffsetY, OffsetXFromMyOrigin, OffsetYFromMyOrigin),
	ord_add_element(CheckedConnections, (AgentName, OffsetXFromMyOrigin, OffsetYFromMyOrigin), CheckedConnectionsWithMe),	

	ord_subtract(AgentConnections_offset, CheckedConnectionsWithMe, NotCheckedConnections),
	
	foldl(
		findNC_folder(ConnectionUpdateList), 
		NotCheckedConnections, 
		(CheckedConnectionsWithMe, FoundWidhtsAndHeights), 
		(AllConnections, MapDimensions)
	),
	
	Output = (AllConnections, MapDimensions).	

	
findCircumferencesFromOffsets(OffsetX, OffsetY, OtherOffsetX, OtherOffsetY, Circumferences) :-
	TC is abs(OtherOffsetX - OffsetX), 
	PC is abs(OtherOffsetY - OffsetY),
	
	(
		(TC > 10, PC > 10, Circumferences = [toroidalCircumference(TC), poloidalCircumference(PC)])
		;
		(TC =< 10, PC > 10, Circumferences = [poloidalCircumference(PC)])
		;
		(TC > 10, PC =< 10, Circumferences = [toroidalCircumference(TC)])
	).
	
	
findBestMapDimensions([], []) :- !.
findBestMapDimensions(FoundMapDimensions, MapDimensions) :-
	findall(PC,
		member(poloidalCircumference(PC), FoundMapDimensions),
		PCs),
	((PCs = [], BestPC = []) ; (PCs = [PC_Head|PC_Tail], foldl(gcd_helper, PC_Tail, PC_Head, PC_GCD), BestPC = [poloidalCircumference(PC_GCD)])),
	
	findall(TC,
		member(toroidalCircumference(TC), FoundMapDimensions),
		TCs),	
	((TCs = [], BestTC = []) ; (TCs = [TC_Head|TC_Tail], foldl(gcd_helper, TC_Tail, TC_Head, TC_GCD), BestTC = [toroidalCircumference(TC_GCD)])),
	
	append(BestPC, BestTC, MapDimensions).
		





shareCommonKnowledge(AllUpdates, NewConnections, NewKnowledge) :-
	shareableKnowledge((MyDispensers, MyGoalCells, MyRoleCells)),
	
	findall((NewDispensers, NewGoalCells, NewRoleCells),
		(
		 member((Agent, Xoffset, Yoffset), NewConnections),
		 member((Agent, _, _, AgentKnowledge, AgentTaskMaster), AllUpdates),

		 (
		 	AgentTaskMaster = [] ; 
		 	(AgentTaskMaster = [TM], not(taskMaster(TM)))
		 ),
		 AgentKnowledge = (AgentDispensers, AgentGoalCells, AgentRoleCells),
		
		 addOffset_list(Xoffset, Yoffset, AgentDispensers, Dispensers),
		 addOffset_list(Xoffset, Yoffset, AgentGoalCells, GoalCells),
		 addOffset_list(Xoffset, Yoffset, AgentRoleCells, RoleCells),
		 
		 ord_subtract(Dispensers, MyDispensers, NewDispensers),
		 ord_subtract(GoalCells, MyGoalCells, NewGoalCells),
		 ord_subtract(RoleCells, MyRoleCells, NewRoleCells)
		),
		
		Knowledge),
		
	foldl(collectListsToSets, Knowledge, ([], [], []), NewKnowledge).


shareableKnowledge(Knowledge) :-
	findall(dispenser(Xd, Yd, Details),
		dispenser(Xd, Yd, Details),
		Dispensers),
	list_to_ord_set(Dispensers, Ord_Dispensers),
		
	findall(goalCell(Xgc, Ygc),
		goalCell(Xgc, Ygc),
		GoalCells),
	list_to_ord_set(GoalCells, Ord_GoalCells),
	
	findall(roleCell(Xrc, Yrc),
		roleCell(Xrc, Yrc),
		RoleCells),
	list_to_ord_set(RoleCells, Ord_RoleCells),
	
	Knowledge = (Ord_Dispensers, Ord_GoalCells, Ord_RoleCells).
	


newConnectionsOrdered(NewConnectionsOrdered) :-
	findall((Agent, OffsetX, OffsetY),
		(
			newConnection(Agent, OffsetX, OffsetY),
			not(agentOffset(Agent, OffsetX, OffsetY))
		),
		NewConnections),
	list_to_ord_set(NewConnections, NewConnectionsOrdered).


oldConnectionsOrdered(OldConnectionsOrdered) :-
	findall((Agent, OffsetX, OffsetY), 
		(
			agentOffset(Agent, OffsetX, OffsetY),
			not(newConnection(Agent, OffsetX, OffsetY))
		), 
		OldConnections),
	list_to_ord_set(OldConnections, OldConnectionsOrdered). 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% dynamic.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%% 
 :- dynamic  
	attachedToMe/2,		% Things attached to the agent 
	enumDirList/2, 		% Pseudo-random list of enumerated directions
	clearEnergyCost/1, clearSteps/1, clearStepsCounter/1, 
	completedClearAction/1,	% Clearing related beliefs
	maxEnergy/1, 		% Maximum energy
	myRole/1,		% Current role
	
	name/1, team/1, teamSize/1, steps/1, role/6,
	
	accepted/1, attached/2, deactivated/1, energy/1, goalZone/2, hit/2, 
	lastAction/1, lastActionResult/1, lastActionParams/1,
	norm/5, roleZone/2, surveyed/4, surveyed/2, score/1, 
	step/1, task/4, thing/4, violation/1,
	
	dispenser/3, goalCell/2, roleCell/2,
	
	myPosition/2,
	
	visited/3,
	
	
	savedCommonEnvironmentPercepts/3,
	
	agentOffset/3,
	
	newConnection/3,
	
	connectionRequest/4,
	connectionUpdate/1,
	connectionToInfo/2, 
	newConnections/3,
		
		
	actionPlan/1, explore/0, fullyEquipped/0, fullyConnected/0, findMapDimensions/0, goTo/2, restart/0,
	
	
	taskMaster/1, submitAgent/1, submitted/1,
	taskPlan/1, taskPlan/7, taskPlanToDo/1,
	
	taskSubmitted/1, dropTask/1, deleteTask/1,
	
	share/1,
		
	resourceRequest/2, resourceRequestSent/3, resourceReply/1, savedResourceReply/1,
	
	connectionFromTo/3, connectionFromTo/5,
	
	occupied/2, blockDelivered/1, waypointsToGoal/2,
	
	translateToMyOrigin_Agent/2, findNC_folder/1, collectListsToSets/0, gcd_helper/0,
	connectedBlocks_folder/0, rankTask/0, buildAgentPlanFromAssignments/1, relativeToAbsolutePositionOfPoints/0. 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% heap.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%% 
/*  Part of SWI-Prolog

    Author:        Lars Buitinck
    E-mail:        larsmans@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2015, Lars Buitinck
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/


/** <module> heaps/priority queues
 *
 * Heaps are data structures that return the entries inserted into them in an
 * ordered fashion, based on a priority. This makes them the data structure of
 * choice for implementing priority queues, a central element of algorithms
 * such as best-first/A* search and Kruskal's minimum-spanning-tree algorithm.
 *
 * This module implements min-heaps, meaning that items are retrieved in
 * ascending order of key/priority. It was designed to be compatible with
 * the SICStus Prolog library module of the same name. merge_heaps/3 and
 * singleton_heap/3 are SWI-specific extension. The portray_heap/1 predicate
 * is not implemented.
 *
 * Although the data items can be arbitrary Prolog data, keys/priorities must
 * be ordered by @=</2. Be careful when using variables as keys, since binding
 * them in between heap operations may change the ordering.
 *
 * The current version implements pairing heaps. These support insertion and
 * merging both in constant time, deletion of the minimum in logarithmic
 * amortized time (though delete-min, i.e., get_from_heap/3, takes linear time
 * in the worst case).
 *
 * @author Lars Buitinck
 */

/*
 * Heaps are represented as heap(H,Size) terms, where H is a pairing heap and
 * Size is an integer. A pairing heap is either nil or a term
 * t(X,PrioX,Sub) where Sub is a list of pairing heaps t(Y,PrioY,Sub) s.t.
 * PrioX @< PrioY. See predicate is_heap/2, below.
 */


add_to_heap(heap(Q0,M),P,X,heap(Q1,N)) :-
    meld(Q0,t(X,P,[]),Q1),
    N is M+1.


delete_from_heap(Q0,P,X,Q) :-
    get_from_heap(Q0,P,X,Q),
    !.
delete_from_heap(Q0,Px,X,Q) :-
    get_from_heap(Q0,Py,Y,Q1),
    delete_from_heap(Q1,Px,X,Q2),
    add_to_heap(Q2,Py,Y,Q).


empty_heap(heap(nil,0)).


singleton_heap(heap(t(X,P,[]), 1), P, X).


get_from_heap(heap(t(X,P,Sub),M), P, X, heap(Q,N)) :-
    pairing(Sub,Q),
    N is M-1.


heap_size(heap(_,N),N).


heap_to_list(Q,L) :-
    to_list(Q,L).
to_list(heap(nil,0),[]) :- !.
to_list(Q0,[P-X|Xs]) :-
    get_from_heap(Q0,P,X,Q),
    heap_to_list(Q,Xs).


is_heap(V) :-
    var(V), !, fail.
is_heap(heap(Q,N)) :-
    integer(N),
    nonvar(Q),
    (   Q == nil
    ->  N == 0
    ;   N > 0,
        Q = t(_,MinP,Sub),
        are_pairing_heaps(Sub, MinP)
    ).

is_pairing_heap(V, _) :-
    var(V),
    !,
    fail.
is_pairing_heap(nil, _).
is_pairing_heap(t(_,P,Sub), MinP) :-
    MinP @=< P,
    are_pairing_heaps(Sub, P).

are_pairing_heaps(V, _) :-
    var(V),
    !,
    fail.
are_pairing_heaps([], _).
are_pairing_heaps([Q|Qs], MinP) :-
    is_pairing_heap(Q, MinP),
    are_pairing_heaps(Qs, MinP).


list_to_heap(Xs,Q) :-
    empty_heap(Empty),
    list_to_heap(Xs,Empty,Q).

list_to_heap([],Q,Q).
list_to_heap([P-X|Xs],Q0,Q) :-
    add_to_heap(Q0,P,X,Q1),
    list_to_heap(Xs,Q1,Q).


min_of_heap(heap(t(X,P,_),_), P, X).


min_of_heap(Q,Px,X,Py,Y) :-
    get_from_heap(Q,Px,X,Q0),
    min_of_heap(Q0,Py,Y).


merge_heaps(heap(L,K),heap(R,M),heap(Q,N)) :-
    meld(L,R,Q),
    N is K+M.


meld(nil,Q,Q) :- !.
meld(Q,nil,Q) :- !.
meld(L,R,Q) :-
    L = t(X,Px,SubL),
    R = t(Y,Py,SubR),
    (   Px @< Py
    ->  Q = t(X,Px,[R|SubL])
    ;   Q = t(Y,Py,[L|SubR])
    ).

pairing([], nil).
pairing([Q], Q) :- !.
pairing([Q0,Q1|Qs], Q) :-
    meld(Q0, Q1, Q2),
    pairing(Qs, Q3),
    meld(Q2, Q3, Q).
	
	
  
 

%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% navigation.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%% 
translate(n, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 - 1.
translate(s, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 + 1.
translate(e, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 + 1.
translate(w, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 - 1.

rotation90(ccw,  1, 0, 0, -1).
rotation90(ccw, 0, -1, -1, 0).
rotation90(ccw, -1, 0, 0, 1).
rotation90(ccw, 0, 1, 1, 0).
rotation90(cw, 0, -1, 1, 0).
rotation90(cw, -1, 0, 0, -1).
rotation90(cw, 0, 1, -1, 0).
rotation90(cw, 1, 0, 0, 1).
rotation180(ccw, 1, 0, -1, 0).
rotation180(ccw, 0, -1, 0, 1).
rotation180(ccw, -1, 0, 1, 0).
rotation180(ccw, 0, 1, 0, -1).
rotation180(cw, 1, 0, -1, 0).
rotation180(cw, 0, -1, 0, 1).
rotation180(cw, -1, 0, 1, 0).
rotation180(cw, 0, 1, 0, -1).

direction(Xr, Yr, D) :-
	translate(D, 0, 0, Xr, Yr).

adjacent(X, Y, Xa, Ya) :-
	translate(_, X, Y, Xa, Ya).
	
rotation(R, Xr, Yr, Xt, Yt, Angle) :- rotation90(R, Xr, Yr, Xt, Yt), Angle = 90.
rotation(R, Xr, Yr, Xt, Yt, Angle) :- rotation180(R, Xr, Yr, Xt, Yt), Angle = 180.

attachedToMe(Xr, Yr, Type, Details) :-
	translate(_, 0, 0, Xr, Yr),
	attachedToMe(Xr, Yr),
	thing(Xr, Yr, Type, Details),
	(Type = block; Type = entity).	

connectedBlocks(Dir, AllConnectedBlocks) :-
	translate(Dir, 0, 0, X, Y),
	connectionFromTo(X, Y, Agent),
	foldl(connectedBlocks_folder, [(X, Y, Agent)],  [(X, Y, Agent)], AllConnectedBlocks).

connectedBlocks_folder((X, Y, Agent), OldConnections, AllConnectedBlocks) :-
	findall((BlockX, BlockY, Agent),
		(
			connectionFromTo(BlockX, BlockY, Agent),
			translate(_, X, Y, BlockX, BlockY),
			not(member((BlockX, BlockY, Agent), OldConnections))
			
		),
		NewConnections),
	list_to_ord_set(NewConnections, NewConnections_ord),
	ord_union(OldConnections, NewConnections_ord, ConnectedBlocks_ord),
	foldl(connectedBlocks_folder, NewConnections_ord,  ConnectedBlocks_ord, AllConnectedBlocks).
	

impassable(X, Y) :- obstacle(X, Y).
impassable(X, Y) :- thing(X, Y, entity, _).
impassable(X, Y) :- thing(X, Y, block, BlockType), not(attachedToMe(X, Y, block, BlockType)).

blocked(D) :- 
	blockedForMe(D) ; blockedForMyAttachments(D).

blockedForMe(D) :-
	translate(D, 0, 0, X, Y),
	impassable(X, Y).
	
blockedForMyAttachments(D) :-
	attachedToMe(X, Y, _, _),
	translate(D, X, Y, X2, Y2),
	impassable(X2, Y2).

blockedRotation(R, Angle) :- 
	attachedToMe(X, Y, _, _),
	(
	   Angle = 90 ->
	      rotation90(R, X, Y, Xr, Yr);
	      rotation180(R, X, Y, Xr, Yr)
	),	
	impassable(Xr, Yr).
	
blockedAfterRotation(R, D) :-
	findall(attachedToMe(Xr, Yr, Type, Details), attachedToMe(Xr, Yr, Type, Details), Attachments),
	rotateAttachments(R, Attachments, AttachmentsRotated),
	member(attachedToMe(Xr, Yr, _, _), AttachmentsRotated),
	translate(D, Xr, Yr, X, Y),
	impassable(X, Y).


rotateAttachments(R, Attachments, AttachmentsRotated) :-
	findall(attachedToMe(Xr, Yr, block, BlockType), 
		(
			member(attachedToMe(X, Y, block, BlockType), Attachments), 
			rotation90(R, X, Y, Xr, Yr)
		), 
		AttachmentsRotated).	
	

availableAttachmentSpot(Xr, Yr) :-
	getAttachments(Attachments),
		
	(
		Attachments = [] ->
			member((Xr, Yr), [(1, 0), (0, 1), (-1, 0), (0, -1)])
			;
			(
				[attachedToMe(Xa, Ya, _, _)] = Attachments,
				rotation180(cw, Xa, Ya, Xr, Yr)
			)
	).


getAttachments(Attachments) :-
	findall(attachedToMe(X, Y, block, BlockType), attachedToMe(X, Y, block, BlockType), Attachments).
	
rotationRequiredToAttach(Xr, Yr) :-
	not(availableAttachmentSpot(Xr, Yr)), availableAttachmentSpot(_, _).
			
			
possibleRotationToAttach(Xr, Yr, R) :-
	availableAttachmentSpot(Xa, Ya),
	rotation(R, Xa, Ya, Xr, Yr, Angle),
	(
		Angle = 90 ->
			not(blockedRotation(R, 90));
			(not(blockedRotation(R, 90)), not(blockedRotation(R, 180)))
	).
	

randomDirection(D) :-
	step(N), name(MyName), nameToNumber(MyName, Num),
	Seed is (N + Num) mod(24), enumDirList(DL, Seed),
	member(D, DL),
	not(blocked(D)).


exploreScore(D, VSum) :-
	Nv = 30,
	step(StepC),
	myPosition(MyX, MyY),
	translate(D, MyX, MyY, X, Y),
	findall(V, 
		(visited(Xv, Yv, StepV), distanceBetweenPoints_Manhattan(X, Y, Xv, Yv, Vd), Vd =< Nv, 
		 	StepDelta is StepC-StepV, StepDelta > 0, V is Vd/(StepDelta*StepDelta)),
		VScoreList),
	listSum(VScoreList, VSum, _).
	
disruptScore(D, VSum) :-
	team(Team),
	translate(D, 0, 0, Xr, Yr),
	findall(Vd, 
		(thing(Xt, Yt, entity, Team), distanceBetweenPoints_Manhattan(Xr, Yr, Xt, Yt, Vd)),
		VScoreList),
	listSum(VScoreList, VSum, _).

distanceScore(D, Xr, Yr, Score) :-
	distMan(0, 0, Xr, Yr, CurrentDistance),
	translate(D, 0, 0, X_new, Y_new),
	distMan(X_new, Y_new, Xr, Yr, NewDistance),
	Score is CurrentDistance - NewDistance.

safeScore(D, Score) :-
	epicenter(Xe, Ye) -> 
		( translate(D, 0, 0, Xr, Yr), distMan(Xr, Yr, Xe, Ye, Dist), Score is 10*Dist ) ;
		( Score = 0 ).

epicenter(X, Y) :-
	findall((Xc, Yc), (thing(Xc, Yc, marker, clear); thing(Xc, Yc, marker, ci)), StrikeZone),
	StrikeZone \= [],
	outerPoints(StrikeZone, [(WestX, _), (_, NorthY), (EastX, _), (_, SouthY)]),
	X is (WestX + EastX)/2,
	Y is (NorthY + SouthY)/2.

outerPoints([Point|Points], OuterPointsList) :-
	outerPoints_iterator(Points, Point, Point, Point, Point, OuterPointsList).
	
outerPoints_iterator([], West, North, East, South, [West, North, East, South]).	
outerPoints_iterator([(X, Y)|Points], (WestX, WestY), (NorthX, NorthY), (EastX, EastY), (SouthX, SouthY), OuterPoints) :-
	(X < WestX  -> West2  = (X, Y); West2 = (WestX, WestY)),
	(Y < NorthY -> North2 = (X, Y); North2 = (NorthX, NorthY)),
	(X > EastX  -> East2  = (X, Y); East2 = (EastX, EastY)),
	(Y > SouthY -> South2 = (X, Y); South2 = (SouthX, SouthY)),
	outerPoints_iterator(Points, West2, North2, East2, South2, OuterPoints).

anyDirection(D) :-
	member(D, [n, s, e, w]).
	
validDirection(D) :-
	anyDirection(D),
	not(blocked(D)).
	
validDirectionAfterRotation(D, R) :-
	anyDirection(D),
	member(R, [cw, ccw]),
	blockedForMyAttachments(D),
	not(blockedRotation(R, 90)),
	not(blockedAfterRotation(R, D)).

validClearingDirection(D, X, Y) :-
	anyDirection(D), blocked(D), translate(D, 0, 0, X, Y), thing(X, Y, obstacle, _).
	
moveDirection(D, Penalty, Action, Params) :-
	(validDirection(D), Penalty = 0, Action = move, Params = [D]) ; 
	(validDirectionAfterRotation(D, R), Penalty = 0.1, Action = rotate, Params = [R]) ; 
	((energy(E), clearEnergyCost(C), E > C) -> (validClearingDirection(D, X, Y), Penalty = 0.2, Action = clear, Params = [X, Y])).

exploreAction(Action, Params) :-
	findall((Score, D, Action, Params),
		 (moveDirection(D, Penalty, Action, Params), 
		  	exploreScore(D, EScore), disruptScore(D, DScore), safeScore(D, SScore), 
		 	Score is EScore + DScore + SScore - Penalty),
	    	 DirectionValueList),
	reverseSort(DirectionValueList, DirectionValueListSorted),
	DirectionValueListSorted = [(MaxScore, _, _, _)|_],
	step(N), Seed is N mod(24), enumDirList(DL, Seed),
	member(Direction, DL), 	member((S, Direction, Action, Params), DirectionValueListSorted), S = MaxScore.


goToAction(Xr, Yr, Action, Params) :-
	findall((Score, D, Action, Params),
		(moveDirection(D, Penalty, Action, Params), 
			exploreScore(D, EScore), distanceScore(D, Xr, Yr, DScore), disruptScore(D, DisruptScore), safeScore(D, SScore), 
			DScore >= 0, Score is (DScore + DisruptScore + EScore + SScore)
		), 
	    	 DirectionValueList),
	reverseSort(DirectionValueList, DirectionValueListSorted),
	DirectionValueListSorted = [(MaxScore, _, _, _)|_],
	step(N), Seed is N mod(24), enumDirList(DL, Seed),
	member(Direction, DL), 	member((S, Direction, Action, Params), DirectionValueListSorted), S = MaxScore.


closestBlockOrDispenserInVision(Xr, Yr, Type, Details) :-
	findall((Dist, Xr, Yr, Type, Details), 
		(
			thing(Xr, Yr, Type, Details),
			not((adjacent(Xr, Yr, Xa, Ya), thing(Xa, Ya, entity, _))),
		 	(
		 		(Type = block, not(attached(Xr, Yr))); 
		 		(Type = dispenser)
		 	),
		 	distMan(0, 0, Xr, Yr, Dist)	
		 ),
		 Things),
	Things \= [],
	sort(Things, SortedThings),
	member((_, Xr, Yr, Type, Details), SortedThings).
	
nearestRoleCell(X, Y) :-
	findall((Dist, Xr, Yr),
		(roleZone(Xr, Yr), distMan(0, 0, Xr, Yr, Dist)),
		RoleCells),
	sort(RoleCells, RoleCellsSorted),
	RoleCellsSorted = [(_, X, Y) |_]. 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% roles.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%% 
 canAttach(Role) :-
 	role(Role, Vision, Actions, Speeds, ClearChance, ClearMaxDistance),
 	member(attach, Actions).
 	
visionForRole(Role, Vision) :-
	role(Role, Vision, _, _, _, _). 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% taskPlanning.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%% 

availableGoalZones(GoalZones) :-
	findall(goalCell(X, Y), goalCell(X, Y), GoalCells),
	goalCellsToGoalZones(GoalCells, [], [], GoalZones).

goalCellsToGoalZones([], _, GoalZones, GoalZones).
goalCellsToGoalZones([goalCell(X1, Y1)|Rest], CheckedCells, FoundZones, GoalZones) :-
	member(goalCell(X2, Y2), CheckedCells), 
	distanceBetweenPoints_Euclidian(X1, Y1, X2, Y2, Dist),
	Dist =< 5,
	!,
	goalCellsToGoalZones(Rest, [goalCell(X1, Y1)|CheckedCells], FoundZones, GoalZones).
	
goalCellsToGoalZones([goalCell(X1, Y1)|Rest], CheckedCells, FoundZones, GoalZones) :-
	goalCellsToGoalZones(Rest, [goalCell(X1, Y1)|CheckedCells], [goalCell(X1, Y1)|FoundZones], GoalZones).

	
	
rankTasks(SortedTasks) :-
	findall(RankedTask, 
		(
			task(Name, Deadline, Reward, Requirements),
			not(resourceRequestSent(task(Name, Deadline, Reward, Requirements), _, _)),
			rankTask(task(Name, Deadline, Reward, Requirements), RankedTask)
		),
		RankedTasks),
	reverseSort(RankedTasks, SortedTasks).
	
rankTask(task(Name, Deadline, Reward, Requirements), RankedTask) :-
	step(CurrentStep),
	RemainingSteps is Deadline - CurrentStep,
	Rank is Reward + RemainingSteps,
	RankedTask = [Rank, task(Name, Deadline, Reward, Requirements)].
	
	
complexTask(Task) :- 
	not(simpleTask(Task)).
	
simpleTask(task(_, _, _, Requirements)) :-
	Requirements = [SingleReq].
	
simpleTask(task(_, _, _, Requirements)) :-
	Requirements = [req(X1, Y1, _), req(X2, Y2, _)],
	(X1 + Y1) =< 1,
	(X2 + Y2) =< 1.	


allResourceReplies(Replies) :-
	findall(Reply,
		(
			savedResourceReply(SavedReply), 
			SavedReply = [Task, Resources, Agent],
			not(occupied(_, Agent)),
			Reply = [Resources, Agent]
		),
		Replies),
	Replies \= [].
		
		
getResourceReplies(Task, GoalCells, Replies) :-
	allResourceReplies(AllReplies),	
	findall((GoalCell, RepliesByGoalCell), 	
		(
			member(GoalCell, GoalCells),
			extractRepliesForGoalCell(GoalCell, AllReplies, RepliesByGoalCell)
		),
		Replies).


extractRepliesForGoalCell(GoalCell, AllReplies, RepliesByGoalCell) :-
	findall((Agent, Resources),
		(
			member(Reply, AllReplies),
			Reply = [ResourcesByGoalCell , Agent],
			member([GoalCell, Resources], ResourcesByGoalCell)			
		),
		RepliesByGoalCell).
			
			
findPlan(Task, Answers, TaskPlan) :-
	bestAssignment(Task, Answers, Assignment, ETA),
	
	buildTaskPlanFromAssignments(Task, Assignment, ETA, TaskPlan).
		


bestAssignment(Task, AllAnswers, BestAssignment, BestETA) :-
	Task = task(_, _, _, Requirements),
    
	requirementsByBlockType(Requirements, ReqByBlockType),
	
	findall(Assignment, 
		( 
		   member((GoalZone, AnswersForGoalZone), AllAnswers),
		   bestAssigntmentForGoalZone(Task, GoalZone, AnswersForGoalZone, ReqByBlockType, Assignment)
		),
		PossibleAssignments),
	
	keysort(PossibleAssignments, [BestETA-BestAssignment|_]).


bestAssigntmentForGoalZone(Task, GoalZone, Answers, Requirements, Assignment) :-
	Task = task(TaskName, Deadline, _, _),
	
	length(Answers, NrOfAnswers),
	length(Requirements, NrOfRequirements),
	NrOfAnswers >= NrOfRequirements,
	
	findall(MaxDistance-Match,
			( 
				matchAgentsToRequirements(TaskName, GoalZone, Answers, Requirements, [], Match),
				findall(Distance, member((_, _, Distance, _, _, _, _), Match), Distances),
				max_list(Distances, MaxDistance)
			),
			Matches),
	
	min_member(BestMatchDistance-BestMatch, Matches),
	
	step(Step),
	ETA is Step + (BestMatchDistance),
	ETA =< Deadline,
	Assignment = ETA-BestMatch.
		
		
		


matchAgentsToRequirements(_, _, [], [_|_], _, []).

matchAgentsToRequirements(_, _,  _, [], PartialMatch, Match) :-
	PartialMatch \= [],
    	Match = PartialMatch.
	
matchAgentsToRequirements(TaskName, GoalZone, Agents, 
                          [(BlockType, ReqQty, Positions)|OtherRequirements],
                          PartialMatch, Match) :-	
			  
	select((Agent, BlockList), Agents, AgentsRest),
	member(Block, BlockList),
	Block = (BlockType, Dist, Stops, BlockQty),
	
	ReqQty =< BlockQty,
	
	agentOffset(Agent, OffsetX, OffsetY),
	addOffset(OffsetX, OffsetY, GoalZone, OffsetGoalZone),
	append(Stops, [OffsetGoalZone], Path),
	AgentPlan = (Agent, TaskName, Dist, BlockType, ReqQty, Path, Positions),
	
	matchAgentsToRequirements(TaskName, GoalZone, AgentsRest, 
				  OtherRequirements, 
				  [AgentPlan |PartialMatch], Match).



requirementsByBlockType(Requirements, FilteredRequirements) :-
	findall((BlockType, 1, [(X, Y)]),
		(
			member(req(X, Y, BlockType), Requirements),
			translate(_, 0, 0, X, Y)
		),
		SubmitAgentRequirements),
	
	findall(req(X, Y, BlockType),
			(
				member(req(X, Y, BlockType), Requirements),
				not(translate(_, 0, 0, X, Y))
			),
			OtherRequirements),
	
	findall(BlockType, member(req(_, _, BlockType), OtherRequirements), BlockTypes),
	setof((BlockType, Qty), (member(BlockType, BlockTypes), count(BlockTypes, BlockType, Qty)), Blocks),
	findall((BlockType, Qty, Positions),
		(
			member((BlockType, Qty), Blocks),
			findall((X, Y), member(req(X, Y, BlockType), OtherRequirements), Positions)
		),
		OtherRequirementsByBlockType),
	
	append(SubmitAgentRequirements, OtherRequirementsByBlockType, FilteredRequirements).	
	
	
buildTaskPlanFromAssignments(Task, Assignments, ETA, TaskPlan) :-
	task(TaskName, _, _, Requirements) = Task,
	
	matchRequirementsAndAssignments(Assignments, Requirements, Connections),

	findall((Agent, TaskName, ETA, AgentPlan, BlockType, BlockQty, Connections),
		member((Agent, TaskName, _, BlockType, BlockQty, AgentPlan, _), Assignments),	
		TaskPlan).


matchRequirementsAndAssignments(Assignments, Requirements, Connections) :-
	findall((Agent, BlockType, Xr, Yr, Dist),
		(
			member(req(Xr, Yr, BlockType), Requirements),
			member((Agent, _, _, BlockType, _, _, Positions), Assignments),
			member((Xr, Yr), Positions),
			distMan(0, 0, Xr, Yr, Dist)	
		),
		Connections).




getResourcesForTask(Task, GoalCells, Resources) :-
	findall(ClosestResources,
		( 
			member(GoalCell, GoalCells), 
		  	closestResourcesToGoalCellForTask(GoalCell, Task, ClosestResources) 
		),
		Resources).
		

closestResourcesToGoalCellForTask(GoalCell, Task, ClosestResourcesToGoalCell) :-
	taskMaster(TaskMaster), 
	agentOffset(TaskMaster, OffsetX, OffsetY),
	addOffset(OffsetX, OffsetY, GoalCell, OffsetGoalCell),
	
	task(_, Deadline, _, Requirements) = Task,
	step(Step),
	MaxDist is (Deadline - Step),
	
	findall(BlockType, member(req(_, _, BlockType), Requirements), RepeatingBlockTypes),
	sort(RepeatingBlockTypes, BlockTypes),

	findall(ClosestResource,
		(
			member(BlockType, BlockTypes), 
			closestBlockResourceToGoalCell(BlockType, OffsetGoalCell, ClosestResource),
			ClosestResource = (BlockType, DistToGoal, WayPoint, Qty),
			DistToGoal < MaxDist
		),
		ClosestResources),
		
	ClosestResources \= [],
	ClosestResourcesToGoalCell = [GoalCell, ClosestResources].
		
		
closestBlockResourceToGoalCell(BlockType, GoalCell, ClosestResource) :-
	findall(_, attachedToMe(_, _, block, BlockType), AttachedBlocks), length(AttachedBlocks, NrOfAttachedBlocks),
	
	(NrOfAttachedBlocks > 0 -> 
	      closestResourcesWithAttachedBlocks(NrOfAttachedBlocks, BlockType, GoalCell, ClosestResource)
	      ;
	      closestResourcesWithoutAttachedBlocks(BlockType, GoalCell, ClosestResource)
	).


closestResourcesWithAttachedBlocks(Qty, BlockType, GoalCell, ClosestResource) :-
	goalCell(X, Y) = GoalCell, myPosition(MyX, MyY), 
	distanceBetweenPoints_Manhattan(MyX, MyY, X, Y, DistToGoal),
	ClosestResource = (BlockType, DistToGoal, [], Qty).


closestResourcesWithoutAttachedBlocks(BlockType, GoalCell, ClosestResource) :-
	Qty is 2,
        closestDispenserToGoal(GoalCell, BlockType, Dispenser, DistWithDispenser),
        ClosestResource = (BlockType, DistWithDispenser, [Dispenser], Qty).
        
        	
closestDispenserToGoal(goalCell(X, Y), BlockType, Dispenser, ShortestDist) :-
	myPosition(MyX, MyY), 
	findall([Distance, dispenser(Xd, Yd, BlockType)],
	                 (
	                 	dispenser(Xd, Yd, BlockType), 
	                 	distanceBetweenPoints_Manhattan(MyX, MyY, Xd, Yd, DistanceToDispenser),
	                 	distanceBetweenPoints_Manhattan(Xd, Yd, X, Y, DistanceFromDispenser),
	                 	Distance is DistanceToDispenser + DistanceFromDispenser
	                 ),
	                 Dispensers),
	         sort(Dispensers, SortedDispensers), 
	         SortedDispensers = [[ShortestDist, Dispenser]|_].     
	        

nextTaskMaster(ConnectedAgents, NextTaskMaster) :-
	name(MyName),
	list_to_ord_set(ConnectedAgents, ConnectedAgents_Ordered),
	ord_add_element(ConnectedAgents_Ordered, MyName, Network),
	[NextTaskMaster|_] = Network.


findCommonTaskMaster(ConnectionUpdateList, NewConnections, CommonTaskMaster) :-
	findall(TaskMaster, taskMaster(TaskMaster), MyTaskMasters),
	
	findall(TaskMaster, 
		(
		 member((Agent, _, _), NewConnections),
		 member((Agent, _, _, _, TaskMasterList), ConnectionUpdateList),
		 member(TaskMaster, TaskMasterList)
		),
		ConnTaskMasters),
	
	list_to_ord_set(ConnTaskMasters, Ord_ConnTaskMasters),
	ord_union(Ord_ConnTaskMasters, MyTaskMasters, TaskMasters),
	
	CommonTaskMaster = TaskMasters.
	


taskMastersOrdered(TaskMasters_Ordered) :-
	findall(TaskMaster, taskMaster(TaskMaster), TaskMasters),
	list_to_ord_set(TaskMasters, TaskMasters_Ordered).
	



	
missingDeliverable(BlockType, Connections) :-
	name(MyName),
	findall(_,
		(
			member((MyName, BlockType, BlockX, BlockY, Dist), Connections),
			not(blockDelivered((MyName, BlockType, BlockX, BlockY, Dist)))
		),
		MissingDeliveries),
	length(MissingDeliveries, NrOfMissingDeliveries),
	findall(_,
		attachedToMe(_, _, block, BlockType),
		Attachments),
	length(Attachments, NrOfAttachments),
	NrOfAttachments < NrOfMissingDeliveries.


finishedStepInTaskPlan(CurrentToDo, _, BlockType, BlockQty, _) :-
	CurrentToDo = dispenser(_, _, _),
	findall(_, attachedToMe(_, _, block, BlockType), Attachments),
	length(Attachments, NrOfAttachedBlocks),
	NrOfAttachedBlocks >= BlockQty.
	
finishedStepInTaskPlan(CurrentToDo, TaskName, _, _, Connections) :-
	CurrentToDo = goalCell(_, _),
	name(MyName),
	(submitAgent(MyName) ->
		submitted(TaskName)
		;
		not((
			member((MyName, BlockType, BlockX, BlockY, Dist), Connections),
			not(blockDelivered((MyName, BlockType, BlockX, BlockY, Dist)))
		)),
		not(connectionFromTo(_, _, _))
	).

deliveredBlock(AgentPlan, Connections, BlockInfo) :-
	name(MyName),
	submitAgent(SubmitAgent),
	connectionFromTo(Xr, Yr, SubmitAgent),
	member(goalCell(Xgc, Ygc), AgentPlan),
	relativePositionOfCoordinatesFromMe(Xgc, Ygc, Xgcr, Ygcr),
	relativePositionOfCoordinatesFromPoint(Xr, Yr, Xgcr, Ygcr, X, Y),
	member((MyName, BlockType, X, Y, Dist), Connections),
	BlockInfo = (MyName, BlockType, X, Y, Dist).
	
	
	
	
	
patternCompleted(Connections) :-
	not((
		member((Agent, BlockType, Xr, Yr, _), Connections), 
		(
			not( attachedToMe(Xr, Yr, block, BlockType) ),
			not( connectionFromTo(_, _, Xr, Yr, Agent) ) 
		)
	)).

	

excessBlock(BlockQty) :-
	findall(attachedToMe(AnyX, AnyY, block, BlockType), attachedToMe(AnyX, AnyY, block, BlockType), Attachments),
	length(Attachments, NrOfAttachments),
	NrOfAttachments > BlockQty.


	
readyToConnect_submitAgent(Connections, ConnectAgent, ConnectFromXr, ConnectFromYr, ConnectToXr, ConnectToYr) :-
	foreach(attachedToMe(Xr, Yr, block, BlockType), member((_, BlockType, Xr, Yr, _), Connections)),
	
	findall(Dist-(Agent, BlockType, Xr, Yr),
		(
			member((Agent, BlockType, Xr, Yr, Dist), Connections),
			(
				not( attachedToMe(Xr, Yr, block, BlockType) ) ,
				not( connectionFromTo(_, _, Xr, Yr, Agent) )
			),
			thing(Xr, Yr, block, BlockType)
		),
		MissingConnections),
	keysort(MissingConnections, [Dist-(ConnectAgent, BlockType, ConnectToXr, ConnectToYr)|_]),
	
	ConnectDist is Dist-1,
	member((_, _, ConnectFromXr, ConnectFromYr, ConnectDist), Connections),
	translate(_, ConnectFromXr, ConnectFromYr, ConnectToXr, ConnectToYr).
	

readyToConnect_otherAgent(GoalXr, GoalYr, BlockXr, BlockYr, BlockType, BlockDist, Connections, ConnectAgent) :-
	attachedToMe(BlockXr, BlockYr, block, BlockType),
	
	
	
	allCloserBlocksInPlace(GoalXr, GoalYr, Connections, BlockDist),
	
	submitAgent(ConnectAgent).
	

allCloserBlocksInPlace(GoalCellXr, GoalCellYr, Connections, BlockDist) :-
	not((
		member((_, BlockType, AnyX, AnyY, Dist), Connections),
		Dist < BlockDist,
		XFromMe is AnyX + GoalCellXr, YFromMe is AnyY + GoalCellYr, 
		not(thing(XFromMe, YFromMe, block, BlockType))
	)).



rotationRequiredTask(AgentXr, AgentYr, BlockType, BlockXr, BlockYr, R) :-
	X is BlockXr - AgentXr, Y is BlockYr - AgentYr,
	not(attachedToMe(X, Y, block, BlockType)),
	attachedToMe(AttachedX, AttachedY, block, BlockType),
	rotation(R, AttachedX, AttachedY, X, Y, Angle),
	(Angle = 90 ->
			not(blockedRotation(R, 90));
			(not(blockedRotation(R, 90)), not(blockedRotation(R, 180)))
	).


nextBlockPosition_submitAgent(Connections, BlockType, BlockXr, BlockYr) :-
	name(MyName),
	member((MyName, BlockType, BlockXr, BlockYr, _), Connections).
	
	
nextBlockPosition_otherAgent(Connections, BlockType, Xr, Yr, Dist) :-
	name(MyName),
	findall(Dist-(MyName, BlockType, BlockXr, BlockYr),
		(
			member((MyName, BlockType, BlockXr, BlockYr, Dist), Connections),
			not(blockDelivered((MyName, BlockType, BlockXr, BlockYr, Dist)))
		),
		MissingConnections),
	keysort(MissingConnections, [Dist-(MyName, BlockType, Xr, Yr)|_]).
	


findClosestAdjacentFreePosition(GoalCellXr, GoalCellYr, Xr, Yr, Connections, GoToX, GoToY) :-
	findall(Dist-(X, Y),
		(
			adjacent(Xr, Yr, X, Y),
			not( impassable(X, Y) ),
			X_GoalPerspective is X - GoalCellXr,
			Y_GoalPerspective is Y - GoalCellYr,
			X_GoalPerspective \= 0,
			Y_GoalPerspective \= 0,
			not(member(((_, _, X_GoalPerspective, Y_GoalPerspective, Dist)), Connections)),
			distanceBetweenPoints_Manhattan(0, 0, X, Y, Dist)
		),
		AdjacentPositions),
	keysort(AdjacentPositions, [Dist-(GoToX, GoToY)|_]).
	
	
		


findWaypoints([], []).
findWaypoints(Path, Waypoints) :-
	Path = [(MyX, MyY), (X, Y)|RemainingPath],
	translate(D, MyX, MyY, X, Y),
	findWaypoints_rec(D, (X, Y), RemainingPath, [], Waypoints).

findWaypoints_rec(_, _, [], Waypoints, Waypoints).
findWaypoints_rec(D, (X, Y), [(X, Y)|RemainingPath], FoundWaypoints, Waypoints) :-
	!,
	findWaypoints_rec(D, (X, Y), RemainingPath, FoundWaypoints, Waypoints).
findWaypoints_rec(D, (X, Y), [(NextX, NextY)|RemainingPath], FoundWaypoints, Waypoints) :-
	translate(D, X, Y, NextX, NextY),
	!,
	findWaypoints_rec(D, (NextX, NextY), RemainingPath, FoundWaypoints, Waypoints).

findWaypoints_rec(_, (X, Y), [(NextX, NextY)|RemainingPath], FoundWaypoints, Waypoints) :-
	translate(D, X, Y, NextX, NextY),
	append(FoundWaypoints, [(X, Y)], NewFoundWaypoints),
	findWaypoints_rec(D, (NextX, NextY), RemainingPath, NewFoundWaypoints, Waypoints).
	
	 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% torus.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%% 
:- dynamic
	poloidalCircumference/1,
	toroidalCircumference/1.
	
torus :-
	poloidalCircumference(_), toroidalCircumference(_).

horizontalCylinder :-
	poloidalCircumference(_), not(toroidalCircumference(_)).
	
verticalCylinder :-
	not(poloidalCircumference(_)), toroidalCircumference(_).
	
rectangle :-
	not(poloidalCircumference(_)), not(toroidalCircumference(_)).
	


relativePositionOfCoordinatesFromMe(X, Y, XFromMe, YFromMe) :-
	myPosition(MyX, MyY),
	relativePositionOfCoordinatesFromPoint(X, Y, MyX, MyY, XFromMe, YFromMe).
		
relativeToAbsolutePositionOfPoints((Xr, Yr), (XFromMyOrigin, YFromMyOrigin)) :-
	relativeToAbsolutePositionOfCoordinates(Xr, Yr, XFromMyOrigin, YFromMyOrigin).
	
relativeToAbsolutePositionOfCoordinates(Xr, Yr, XFromMyOrigin, YFromMyOrigin) :-
	myPosition(MyX, MyY),
	X is Xr + MyX,
	Y is Yr + MyY,	
	translateCoordinatesToMyOrigin(X, Y, XFromMyOrigin, YFromMyOrigin).
	
translateCoordinatesToMyOrigin(X, Y, XFromMyOrigin, YFromMyOrigin) :-
	relativePositionOfCoordinatesFromPoint(X, Y, 0, 0, XFromMyOrigin, YFromMyOrigin).
	
relativePositionOfCoordinatesFromPoint(X, Y, PointX, PointY, XFromPoint, YFromPoint) :-
	distanceFromSourceToDestinationByDimension(PointX, PointY, X, Y, XFromPoint, YFromPoint).
	
distanceBetweenPoints_Euclidian(Point1X, Point1Y, Point2X, Point2Y, Distance) :-
	distanceFromSourceToDestinationByDimension(Point1X, Point1Y, Point2X, Point2Y, Dx, Dy),
	Distance is sqrt((Dx^2) + (Dy^2)).

distanceBetweenPoints_Manhattan(Point1X, Point1Y, Point2X, Point2Y, Distance) :-
	distanceFromSourceToDestinationByDimension(Point1X, Point1Y, Point2X, Point2Y, Dx, Dy),
	Distance is abs(Dx) + abs(Dy).
	
distanceFromSourceToDestinationByDimension(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	torus ->
		distanceFromSourceToDestinationByDimension_torus(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy)
	;
	horizontalCylinder ->
		distanceFromSourceToDestinationByDimension_horizontalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy)
	;
	verticalCylinder ->
		distanceFromSourceToDestinationByDimension_verticalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy)
	;
	rectangle ->
		distanceFromSourceToDestinationByDimension_rectangle(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy)
	;
		fail.	
	
distanceFromSourceToDestinationByDimension_torus(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	distanceFromSourceToDestinationByDimension_horizontalCylinder(SourceX, SourceY, DestinationX, DestinationY, _, Dy),
	distanceFromSourceToDestinationByDimension_verticalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, _).
	
distanceFromSourceToDestinationByDimension_horizontalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	poloidalCircumference(PC),
	Dx is DestinationX - SourceX,
	NaiveDy is (DestinationY - SourceY) mod PC,
	(
		(NaiveDy > PC/2, Dy is NaiveDy - PC) 
		;
		(NaiveDy < -(PC/2), Dy is PC + NaiveDy)
		;
		(NaiveDy =< PC/2, NaiveDy >= -(PC/2), Dy = NaiveDy)
	).
	
distanceFromSourceToDestinationByDimension_verticalCylinder(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	toroidalCircumference(TC),
	NaiveDx is (DestinationX - SourceX) mod	TC,
	(
		(NaiveDx > TC/2, Dx is NaiveDx - TC) 
		;
		(NaiveDx < -(TC/2), Dx is TC + NaiveDx)
		;
		(NaiveDx =< TC/2, NaiveDx >= -(TC/2), Dx = NaiveDx)
	),
	Dy is DestinationY - SourceY.
	
distanceFromSourceToDestinationByDimension_rectangle(SourceX, SourceY, DestinationX, DestinationY, Dx, Dy) :-
	Dx is DestinationX - SourceX,
	Dy is DestinationY - SourceY. 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% utility.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%% 


obstacle(Xr, Yr) :-
	thing(Xr, Yr, obstacle, _).
			
nameToSeed(Name, Seed) :- 									    
 	sub_string(Name, B, L, _, "agentGOAL-DTU"),
 	BL is B+L,
 	sub_string(Name, BL, _, 0, Rest),
 	number_string(SeedX, Rest),
 	Seed is ((SeedX-1)*3+5) mod 24.


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
 	

abs(X1, X2) :- X1 < 0, !, X2 is -X1.
abs(X, X).


distMan(X1, Y1, X2, Y2, Res) :- X is X2-X1, abs(X, XAbs), Y is Y2-Y1, abs(Y, YAbs), Res is (XAbs+YAbs).
distEuc(X1, Y1, X2, Y2, D) :- D is sqrt((X2-X1)^2 + (Y2-Y1)^2).

reverseSort(List, ListSortedRev) :- sort(List, ListSorted), rev(ListSorted, ListSortedRev).
rev([], []).
rev([H|T], R) :- rev(T, Rt), append(Rt, [H], R). 


listSum([], 0, 0).
listSum([H|T], Sum, Length) :-
    listSum(T, TailSum, TailLength),
    Sum is H + TailSum,
    Length is 1 + TailLength.



generateEnumDirList(P, V, InitialSeed) :-
	findall(Px, permutation([n, e, s, w], Px), PL),
	generateEnumDirList(PL, InitialSeed, P, V).
generateEnumDirList([P|_], V, P, V).
generateEnumDirList([_|PL], C, P, V) :-
	Cx is (C + 1),
	Cxx is Cx mod 24,
	generateEnumDirList(PL, Cxx, P, V). 

	
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

translateToMyOrigin_Agent(OffsetX, OffsetY, (Agent, AgentX, AgentY), ResultingAgent) :-
	X is AgentX + OffsetX,
	Y is AgentY + OffsetY,
	translateCoordinatesToMyOrigin(X, Y, XFromMyOrigin, YFromMyOrigin),
	ResultingAgent = (Agent, XFromMyOrigin, YFromMyOrigin). 
 


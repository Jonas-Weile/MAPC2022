% Find all save connection requests from specific step
getAllConnectionRequestsFromStep(Step, ConnectionRequests) :-
	findall([AgentName, AgentX, AgentY, CEPs, Step],
		connectionRequest(AgentName, AgentX, AgentY, CEPs, Step),
		ConnectionRequests).

% Order all the connected agents
connectedAgentsOrdered(ConnectedAgents_ord) :-
	findall(Agent, agentOffset(Agent, _, _), Agents),
	list_to_ord_set(Agents, Agents_ord),
	name(MyName),
	ord_add_element(Agents_ord, MyName, ConnectedAgents_ord).
	

% Collect all environmentPercepts in the vision range of the other agent at (X, Y).
% These can be things I can see, or permanent things I know of.  	
identifyCommonEnvironmentPercepts(X, Y, Cep) :-
	visionForRole(default, VisionRange),
	findall(envPercept(Xe, Ye, Type), 
		(  
		   (  
		      thing(Xe, Ye, Type, _);
		      (obstacle(Xe, Ye), Type = obstacle);
		      (goalZone(Xe, Ye), Type = goalZone);
		      (roleZone(Xe, Ye), Type = roleZone)
		   ),
		   % Exclude the agent itself
		   (Xe, Ye) \= (X, Y),
		   
		   % Only condsider things within the visionrange of both agents
		   distanceBetweenPoints_Manhattan(X, Y, Xe, Ye, D1), D1 =< VisionRange,
		   distanceBetweenPoints_Manhattan(0, 0, Xe, Ye, D2), D2 =< VisionRange		   
		), 
		Cep
	       ).
		

uniqueConnectionRequests(Requests, UniqueRequests) :-
	% First, find all nonempty Requests
	findall((Agent, AgentX, AgentY, Xr, Yr, CEP),
		(
		   member(Request, Requests),
		   Request = [Agent, AgentX, AgentY, CEPs, _],
		   member([Xr, Yr, CEP], CEPs),
		   CEP \= []
		),
		NonEmptyRequests),
	
	% Now, filter out any request with duplicates
	findall(Request,
		(
		   member((Agent, AgentX, AgentY, Xr, Yr, CEP), NonEmptyRequests),
		   not(( 
		   	member((OtherAgent, _, _, Xr, Yr, OtherCEP), NonEmptyRequests),
		   	OtherAgent \= Agent,
		   	sort(CEP, CEPsorted),
		   	sort(OtherCEP, CEPsorted)
		   )),
		   Request = (Agent, AgentX, AgentY, Xr, Yr, CEP)
		),
		UniqueRequests).
		

		
matchingEnvironmentPercepts([], [], _, _).
matchingEnvironmentPercepts([envPercept(Xe, Ye, Type)|Cep1], Cep2, X, Y) :-
	Xe2 is Xe+X, Ye2 is Ye+Y,
	select(envPercept(Xe2, Ye2, Type), Cep2, Cep2Rest),
	matchingEnvironmentPercepts(Cep1, Cep2Rest, X, Y).


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
	
	% Find all my new and old connections
	member((AgentName, AgentNewConnections, AgentOldConnections, _, _), ConnectionUpdateList),
	ord_union(AgentNewConnections, AgentOldConnections, AgentConnections),
	
	% Add offset
	maplist(translateToMyOrigin_Agent(OffsetX, OffsetY), AgentConnections, AgentConnections_offset),
	 
	% Add the agent itself to the checked connections
	translateCoordinatesToMyOrigin(OffsetX, OffsetY, OffsetXFromMyOrigin, OffsetYFromMyOrigin),
	ord_add_element(CheckedConnections, (AgentName, OffsetXFromMyOrigin, OffsetYFromMyOrigin), CheckedConnectionsWithMe),	

	% Extract all unchecked connections
	ord_subtract(AgentConnections_offset, CheckedConnectionsWithMe, NotCheckedConnections),
	
	% Recursive call
	foldl(
		findNC_folder(ConnectionUpdateList), 
		NotCheckedConnections, 
		(CheckedConnectionsWithMe, FoundWidhtsAndHeights), 
		(AllConnections, MapDimensions)
	),
	
	Output = (AllConnections, MapDimensions).	

	
findCircumferencesFromOffsets(OffsetX, OffsetY, OtherOffsetX, OtherOffsetY, Circumferences) :-
	% Add the found ToroidalCircumference, PoloidalCircumference or both
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
		





% Find new knowledge from new connections	
shareCommonKnowledge(AllUpdates, NewConnections, NewKnowledge) :-
	% My knowledge
	shareableKnowledge((MyDispensers, MyGoalCells, MyRoleCells)),
	
	% Find knowledge
	findall((NewDispensers, NewGoalCells, NewRoleCells),
		(
		 % Find the Dispensers, GoalCells RoleCells of new Connections
		 member((Agent, Xoffset, Yoffset), NewConnections),
		 member((Agent, _, _, AgentKnowledge, AgentTaskMaster), AllUpdates),

		 % Only check new agents with whom we do not share a taskmaster
		 (
		 	AgentTaskMaster = [] ; 
		 	(AgentTaskMaster = [TM], not(taskMaster(TM)))
		 ),
		 AgentKnowledge = (AgentDispensers, AgentGoalCells, AgentRoleCells),
		
		 % Add offset to knowledge
		 addOffset_list(Xoffset, Yoffset, AgentDispensers, Dispensers),
		 addOffset_list(Xoffset, Yoffset, AgentGoalCells, GoalCells),
		 addOffset_list(Xoffset, Yoffset, AgentRoleCells, RoleCells),
		 
		 % Only get new information
		 ord_subtract(Dispensers, MyDispensers, NewDispensers),
		 ord_subtract(GoalCells, MyGoalCells, NewGoalCells),
		 ord_subtract(RoleCells, MyRoleCells, NewRoleCells)
		),
		
		Knowledge),
		
	foldl(collectListsToSets, Knowledge, ([], [], []), NewKnowledge).


% Knowledge is made of three lists - dispensers, goalcells and rolecells
shareableKnowledge(Knowledge) :-
	% find all dispensers
	findall(dispenser(Xd, Yd, Details),
		dispenser(Xd, Yd, Details),
		Dispensers),
	list_to_ord_set(Dispensers, Ord_Dispensers),
		
	% find all goalcells
	findall(goalCell(Xgc, Ygc),
		goalCell(Xgc, Ygc),
		GoalCells),
	list_to_ord_set(GoalCells, Ord_GoalCells),
	
	% find all rolecells
	findall(roleCell(Xrc, Yrc),
		roleCell(Xrc, Yrc),
		RoleCells),
	list_to_ord_set(RoleCells, Ord_RoleCells),
	
	% append everything into knowledge list.
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
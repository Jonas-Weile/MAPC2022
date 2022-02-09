:- dynamic
 
 	%%%%%% COMMON %%%%%%
	name/1, team/1, teamSize/1, steps/1, vision/1, clearEnergyCost/1, maxEnergy/1, 
	clearing/2, clearSteps/1, clearStepsCounter/1, completedClearAction/1, role/6,
	
	% Environment percepts
	step/1, score/1, lastAction/1, lastActionResult/1, lastActionParams/1,
	energy/1, deactivated/1, task/4, attached/2,
	thing/4, accepted/1, role/1, goalZone/2, roleZone/2, norm/6, violation/1,
	
	% Things
	taskboard/2, dispenser/3,
	
	% Internal percepts
	goalCell/2, roleCell/2,
	
	% myPosition(?X, ?Y) - Current position of agent
	myPosition/2,
	
	% visited(?X, ?Y, ?Step)
	visited/3,
	
	enumDirList/2, attachedToMe/2,
	
	
	%%%%%% Communiaction %%%%%%
	commonEnvironmentPercepts/3,
	savedCommonEnvironmentPercepts/3,
	
	% agentOffset(Agent, OffsetX, OffsetY)
	agentOffset/3,
	
	% Simply used to temporarily store new connections
	newConnection/3,
	
	%connectionUpdate/2,
	connectionUpdate/1, 
	newConnections/3,
	connectionRequest/5,
	connectionToInfo/2,
		
		
	%%%%%% GOALS %%%%%%
	explore/0, fullyEquipped/0, fullyConnected/0, findMapDimensions/0, restart/0, goTo/2,
	actionPlan/1,
	
	
	%%%%%% PLANNING %%%%%%
	taskMaster/1, submitAgent/1, submitted/1,
	taskPlan/1, taskPlan/7, taskPlanToDo/1,
	
	% Messages
	taskSubmitted/1, dropTask/1, deleteTask/1,
	
	% Used to mark information to share if I'm part of a team
	share/1,
		
	% resource requests
	resourceRequest/2, resourceRequestSent/2, resourceReply/1, savedResourceReply/1,
	
	% occupied thing by some agent
	occupied/2, taskTaken/2, checkedTask/1,
	
	% connectionFromTo
	connectionFromTo/3, connectionFromTo/5, blockDelivered/1,
	
	
	waypointsToGoal/2,
	
	%%%%%%%%%% THESE ARE SIMPLY TO STOP GOAL FROM COMPLAINING %%%%%%%%%
	translateToMyOrigin_Agent/2, findNC_folder/1, collectListsToSets/0, gcd_helper/0,
	connectedBlocks_folder/0, rankTask/0, buildAgentPlanFromAssignments/1, relativeToAbsolutePositionOfPoints/0.			
			
			
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NAVIGATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% RLATIVE NAVIGATION %%%%%%%%%%%%%%%%%%
% Block in sight
blockInView(Xr, Yr, Type) :-
	thing(Xr, Yr, block, Type),
	not(attached(Xr, Yr)).
	
dispenserInView(Xr, Yr, Type) :-
	thing(Xr, Yr, dispenser, Type).

adjacentPositions(X, Y, Xa, Ya) :-
	translate(_, X, Y, Xa, Ya).
	
myAdjacentPositions(Xa, Ya) :-
	adjacentPositions(0, 0, Xa, Ya).

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
	
	
% The locations around (X, Y) has no agents.
agentFreeRelativeLocation(Xr, Yr) :-
	not((adjacentPositions(Xr, Yr, Xa, Ya), thing(Xa, Ya, entity, _))).


%%%%% MOVING %%%%%
% Moving in a given direction 
translate(n, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 - 1.
translate(s, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 + 1.
translate(e, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 + 1.
translate(w, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 - 1.


% Direction of relative position
direction(Xr, Yr, D) :-
	translate(D, 0, 0, Xr, Yr).

% Is direction blocked
blocked(D) :- 
	blockedForMe(D) ; blockedForMyAttachments(D).

blockedForMe(D) :-
	translate(D, 0, 0, X, Y),
	impassable(X, Y).
	
blockedForMyAttachments(D) :-
	attachedToMe(X, Y, _, _),
	translate(D, X, Y, X2, Y2),
	impassable(X2, Y2).
	
blockedAfterRotation(R, D) :-
	findall(attachedToMe(Xr, Yr, Type, Details), attachedToMe(Xr, Yr, Type, Details), Attachments),
	rotateAttachmentsAttached(R, Attachments, AttachmentsRotated),
	member(attachedToMe(Xr, Yr, _, _), AttachmentsRotated),
	translate(D, Xr, Yr, X, Y),
	impassable(X, Y).
	
	
impassable(X, Y) :- obstacle(X, Y).
impassable(X, Y) :- thing(X, Y, entity, _).
impassable(X, Y) :- thing(X, Y, block, BlockType), not(attachedToMe(X, Y, block, BlockType)).


% Infer needed rotation to point attachment spot (Xa, Ya) towards (Xt, Yt)
% other direction
rotation(R, Xa, Ya, Xt, Yt, Angle) :- rotation90(R, Xa, Ya, Xt, Yt), Angle = 90.
rotation(R, Xa, Ya, Xt, Yt, Angle) :- rotation180(R, Xa, Ya, Xt, Yt), Angle = 180.

% 90 degrees
rotation90(ccw,  1, 0, 0, -1).
rotation90(ccw, 0, -1, -1, 0).
rotation90(ccw, -1, 0, 0, 1).
rotation90(ccw, 0, 1, 1, 0).
rotation90(cw, 0, -1, 1, 0).
rotation90(cw, -1, 0, 0, -1).
rotation90(cw, 0, 1, -1, 0).
rotation90(cw, 1, 0, 0, 1).
% 180 degrees
rotation180(ccw, 1, 0, -1, 0).
rotation180(ccw, 0, -1, 0, 1).
rotation180(ccw, -1, 0, 1, 0).
rotation180(ccw, 0, 1, 0, -1).
rotation180(cw, 1, 0, -1, 0).
rotation180(cw, 0, -1, 0, 1).
rotation180(cw, -1, 0, 1, 0).
rotation180(cw, 0, 1, 0, -1).

% Can rotate
blockedRotation(R, Angle) :- 
	attachedToMe(X, Y, _, _),
	(
	   Angle = 90 ->
	      rotation90(R, X, Y, Xr, Yr);
	      rotation180(R, X, Y, Xr, Yr)
	),	
	impassable(Xr, Yr).
	
	
% availableAttachmentSpots - MAKE IT SUCH THAT AN AGENT CAN ONLY ATTACH TWO BLOCKS!
% availableAttachmentSpots - MAKE IT SUCH THAT AN AGENT CAN ONLY ATTACH TWO BLOCKS!
availableAttachmentSpots(X, Y) :- 
	% Find all attachments
	findall(attachedToMe(Xa, Ya, Type, Details),
		attachedToMe(Xa, Ya, Type, Details),
		Attachments),
		
	% If nothing attached, then any spot is fine
	(
		Attachments = [] ->
			member((X, Y), [(1, 0), (0, 1), (-1, 0), (0, -1)])
			;
			% Otherwise, it should be opposite the block already attached
			(
				[attachedToMe(Xa, Ya, Type, Details)] = Attachments,
				rotation180(cw, Xa, Ya, X, Y)
			)
	).

	
	
% Cannot attach to block until it has rotated
rotationRequiredToAttach(Xr, Yr) :-
	not(availableAttachmentSpots(Xr, Yr)), availableAttachmentSpots(_, _).
			
% Find a possible rotation
possibleRotation(Xr, Yr, R) :-
	availableAttachmentSpots(Xa, Ya),
	rotation(R, Xa, Ya, Xr, Yr, Angle),
	% check if we have to rotate 90 or 180 degrees.
	(
		Angle = 90 ->
			not(blockedRotation(R, 90));
			(not(blockedRotation(R, 90)), not(blockedRotation(R, 180)))
	).

% Find the closest block/dispenser in vision
closestBlockOrDispenserInVision(Xr, Yr, Type, Details) :-
	findall((Dist, Xr, Yr, Type, Details), 
		(
			thing(Xr, Yr, Type, Details),
			agentFreeRelativeLocation(Xr, Yr),
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
	

randomDirection(D) :-
	step(N), name(MyName), nameToNumber(MyName, Num),
	Seed is (N + Num) mod(24), enumDirList(DL, Seed),
	member(D, DL),
	not(blocked(D)).

% Does pretty much the same thing as goToDirections, but can also rotate!
goToAction(Xr, Yr, Action) :-
	findall((Score, D), 
		(
			(validDirection(D); validDirectionAfterRotation(D)), 
			visitedScore(D, VScore), clearScore(D, CScore), positionScore(D, Xr, Yr, PScore), 
			Score is ((VScore+2*PScore)-CScore)), 
		DirectionValueList),
	sort(DirectionValueList, DirectionValueListSorted),
	member((_, D), DirectionValueListSorted),
	(
		validDirection(D) ->
			( constructiveMove(Xr, Yr, D), Action = move(D) ) ;
			( member(R, [cw, ccw]), not(blockedRotation(R, 90)), Action = rotate(R) )
	).

goToDirections(Xb, Yb, D) :-
	findall((Score, D), (validDirection(D), visitedScore(D, VScore), clearScore(D, CScore), 
		positionScore(D, Xb, Yb, PScore), Score is ((VScore+PScore)-CScore)), DirectionValueList),
	sort(DirectionValueList, DirectionValueListSorted),
	member((_, D), DirectionValueListSorted).
	
	
% Score based on distance to target position given direction D 
positionScore(D, Xb, Yb, PScore) :-
	translate(D, 0, 0, X, Y),
	distanceBetweenPoints_Manhattan(X, Y, Xb, Yb, PScore).
	
% visitedScore
visitedScore(D, VScore) :-
	myPosition(Xr, Yr),
	step(CurrentStep),
	translate(D, Xr, Yr, X, Y),
	translateCoordinatesToMyOrigin(X, Y, XFromMyOrigin, YFromMyOrigin),
	findall(_, 
		(
			visited(XFromMyOrigin, YFromMyOrigin, Step),
			5 < CurrentStep-Step
		), 
		VisitedList),
	length(VisitedList, VScore1),
	VScore is sqrt(VScore1).
	
% detachScore
detachScore(D, Score) :-
	exploreScore(D, ExScore),
	translate(D, 0, 0, X1, Y1),
	findall(L, (obstacle(X, Y), distMan(X1, Y1, X, Y, L)), Ls),
	listSum(Ls, DScore, _),
	Score is ExScore+DScore.
	
	
	
% All directions
validDirection(D) :-
	member(D, [n, s, e, w]),
	not(blocked(D)).
	
validDirectionAfterRotation(D) :-
	member(D, [n, s, e, w]),
	blocked(D),
	attachedToMe(_, _, _, _),
	blockedForMyAttachments(D),
	not(blockedRotation(R, 90)),
	not(blockedAfterRotation(R, D)).

% Heuristic
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
	
	
safeScore(D, VSum) :-
	team(Team),
	translate(D, 0, 0, X, Y),
	findall(Vd, 
		((thing(Xt, Yt, entity, Team) ; goalZone(Xt, Yt) ; thing(Xt, Yt, dispenser, _)), 
		distanceBetweenPoints_Manhattan(X, Y, Xt, Yt, Vd), Vd > 0),
		VScoreList),
	listSum(VScoreList, VSum, _).


clearScore(D, Score) :-
	translate(D, 0, 0, Xr, Yr),
	epicenter(Xe, Ye) ->
		(distMan(Xr, Yr, Xe, Ye, Dist), Score is 10*(Dist+1));
		(Score = 0).
	
	

% safeExploreDirections (prioritize avoiding disrupting agents working on turning in tasks
safeExploreDirections(Direction) :-
	findall((Score, D),
		 (validDirection(D), exploreScore(D, EScore), safeScore(D, SScore), clearScore(D, CScore),
	    		Score is EScore + SScore + CScore),
	    	 DirectionValueList),
	reverseSort(DirectionValueList, DirectionValueListSorted),
	DirectionValueListSorted = [(MaxScore, _)|_],
	step(N), Seed is N mod(24), enumDirList(DL, Seed),
	member(Direction, DL), 	member((V, Direction), DirectionValueListSorted), V = MaxScore.


exploreDirections(Direction) :-
	findall((EScore, D),
		 (validDirection(D), exploreScore(D, EScore)),
	    	 DirectionValueList),
	reverseSort(DirectionValueList, DirectionValueListSorted),
	DirectionValueListSorted = [(MaxScore, _)|_],
	step(N), Seed is N mod(24), enumDirList(DL, Seed),
	member(Direction, DL), 	member((V, Direction), DirectionValueListSorted), V = MaxScore.


% Find the epicenter of a clear event
epicenter(X, Y) :-
	findall((Xc, Yc), (thing(Xc, Yc, marker, clear); thing(Xc, Yc, marker, ci)), StrikeZone),
	StrikeZone \= [],
	outerPoints(StrikeZone, [(WestX, _), (_, NorthY), (EastX, _), (_, SouthY)]),
	X is (WestX + EastX)/2,
	Y is (NorthY + SouthY)/2.
	


% Find the outermost points (that defines the perimeter) from a given list.
outerPoints([Point|Points], OuterPointsList) :-
	outerPoints_iterator(Points, Point, Point, Point, Point, OuterPointsList).
outerPoints_iterator([], West, North, East, South, [West, North, East, South]).	
outerPoints_iterator([(X, Y)|Points], (WestX, WestY), (NorthX, NorthY), (EastX, EastY), (SouthX, SouthY), OuterPoints) :-
	(X < WestX  -> West2  = (X, Y); West2 = (WestX, WestY)),
	(Y < NorthY -> North2 = (X, Y); North2 = (NorthX, NorthY)),
	(X > EastX  -> East2  = (X, Y); East2 = (EastX, EastY)),
	(Y > SouthY -> South2 = (X, Y); South2 = (SouthX, SouthY)),
	outerPoints_iterator(Points, West2, North2, East2, South2, OuterPoints).

	



addOffset(OffsetX, OffsetY, Input, Output) :-
	Input = taskboard(Xt, Yt) 	-> 
		(offsetAndTranslateToMyOrigin(OffsetX, OffsetY, Xt, Yt, XFromMyOrigin, YFromMyOrigin),
		 Output = taskboard(XFromMyOrigin, YFromMyOrigin)) 
	;
	Input = dispenser(Xd, Yd, Type) -> 
		(offsetAndTranslateToMyOrigin(OffsetX, OffsetY, Xd, Yd, XFromMyOrigin, YFromMyOrigin),
		 Output = dispenser(XFromMyOrigin, YFromMyOrigin, Type)) 
	;
	(Input = goalCell(Xgc, Ygc),
		(offsetAndTranslateToMyOrigin(OffsetX, OffsetY, Xgc, Ygc, XFromMyOrigin, YFromMyOrigin),
		 Output = goalCell(XFromMyOrigin, YFromMyOrigin))).


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
	
	
    
%%%%%%%%%%%%%%%%% TORUS %%%%%%%%%%%%%%%%%%%%%%
% Imagine a donut torus - poloidal direction is the the vertical 'through the hole' direction
% toroidal direction is then 'around the donut' or moving horizontally.
:- dynamic
	poloidalCircumference/1,
	toroidalCircumference/1.
	
torus :-
	poloidalCircumference(_), toroidalCircumference(_).

% The cylinder is "lying on its' side"
horizontalCylinder :-
	poloidalCircumference(_), not(toroidalCircumference(_)).
	
% The cylinder is "standing upright"
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
	
	

	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONNECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getAllConnectionRequestsFromStep(Step, ConnectionRequests) :-
	findall([AgentName, AgentX, AgentY, CEPs, Step],
		connectionRequest(AgentName, AgentX, AgentY, CEPs, Step),
		ConnectionRequests).

connectedAgentsOrdered(ConnectedAgents_ord) :-
	findall(Agent, agentOffset(Agent, _, _), Agents),
	list_to_ord_set(Agents, Agents_ord),
	name(MyName),
	ord_add_element(Agents_ord, MyName, ConnectedAgents_ord).
	


% Collect all environmentPercepts in the vision range of the other agent at (X, Y).
% These can be things I can see, or permanent things I know of.  	
identifyCommonEnvironmentPercepts(X, Y, Cep) :-
	vision(VisionRange),
	findall(envPercept(Xe, Ye, Type), 
		(  
		   (  
		      thing(Xe, Ye, Type, _);
		      (obstacle(Xe, Ye), Type = obstacle);
		      (goalZone(Xe, Ye), Type = goal)	      
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
	shareableKnowledge((MyTaskboards, MyDispensers, MyGoalCells)),
	
	% Find knowledge
	findall((NewTaskboards, NewDispensers, NewGoalCells),
		(
		 % Find the Taskboards, Dispensers and GoalCells of new Connections
		 member((Agent, Xoffset, Yoffset), NewConnections),
		 member((Agent, _, _, AgentKnowledge, AgentTaskMaster), AllUpdates),

		 % Only check new agents with whom we do not share a taskmaster
		 (
		 	AgentTaskMaster = [] ; 
		 	(AgentTaskMaster = [TM], not(taskMaster(TM)))
		 ),
		 AgentKnowledge = (AgentTaskboards, AgentDispensers, AgentGoalCells),
		
		 % Add offset to knowledge
		 addOffset_list(Xoffset, Yoffset, AgentTaskboards, Taskboards),
		 addOffset_list(Xoffset, Yoffset, AgentDispensers, Dispensers),
		 addOffset_list(Xoffset, Yoffset, AgentGoalCells, GoalCells),
		 
		 % Only get new information
		 ord_subtract(Taskboards, MyTaskboards, NewTaskboards),
		 ord_subtract(Dispensers, MyDispensers, NewDispensers),
		 ord_subtract(GoalCells, MyGoalCells, NewGoalCells)
		),
		
		Knowledge),
		
	foldl(collectListsToSets, Knowledge, ([], [], []), NewKnowledge).


% Knowledge is made of three lists - taskboards, dispensers and goalcells
shareableKnowledge(Knowledge) :-
	% find all taskboards
	findall(taskboard(Xt, Yt),
		taskboard(Xt, Yt),
		Taskboards),
	list_to_ord_set(Taskboards, Ord_Taskboards),

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
	
	% append everything into knowledge list.
	Knowledge = (Ord_Taskboards, Ord_Dispensers, Ord_GoalCells).
	
	




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

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PLANNING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	
finishedStepInTaskPlan(CurrentToDo, TaskName, _, _, _) :-
	CurrentToDo = taskboard(_, _),
	accepted(TaskName).

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
	% Check that all my attachments are in place
	foreach(attachedToMe(Xr, Yr, block, BlockType), member((_, BlockType, Xr, Yr, _), Connections)),
	
	% Find the next connection, and ensure it is in place
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
	
	% Find the block it should connect to
	ConnectDist is Dist-1,
	member((_, _, ConnectFromXr, ConnectFromYr, ConnectDist), Connections),
	translate(_, ConnectFromXr, ConnectFromYr, ConnectToXr, ConnectToYr).
	

readyToConnect_otherAgent(GoalXr, GoalYr, BlockXr, BlockYr, BlockType, BlockDist, Connections, ConnectAgent) :-
	% make sure I have an attachment in the correct place
	attachedToMe(BlockXr, BlockYr, block, BlockType),
	
	% Make sure I am not in the way of others
	
	
	% Make sure all blocks closer to the submit agent are in place
	allCloserBlocksInPlace(GoalXr, GoalYr, Connections, BlockDist),
	
	% Connect the block to the submitagent
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
	% Find number of attachments
	findall(Dist-(X, Y),
		(
			translate(_, Xr, Yr, X, Y),
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
	
	
	
%%% PLANNING - TASKMASTER %%%%
goalZoneFromTaskPlan(TaskPlan, GoalZone) :-
	TaskPlan = [(Agent, _, _, AgentPlan, _, _, _)|_],
	member(goalCell(Xgc, Ygc), AgentPlan),
	agentOffset(Agent, OffsetX, OffsetY),
	X is Xgc + OffsetX,
	Y is Ygc + OffsetY,
	translateCoordinatesToMyOrigin(X, Y, XFromMyOrigin, YFromMyOrigin),
	GoalZone = goalCell(XFromMyOrigin, YFromMyOrigin).

% Find distinct goalcells
availableGoalZones(GoalZones) :-
	findall(goalCell(X, Y), goalCell(X, Y), GoalCells),
	goalCellsToAvailableGoalZones(GoalCells, GoalZones).

goalCellsToAvailableGoalZones(GoalCells, AvailableGoalZones) :-
	findall(goalCell(X, Y), (goalCell(X, Y), occupied(_, goalCell(X, Y))), UnavailableGoalCells),
	availableGoalCellsToGoalZones_rec(GoalCells, UnavailableGoalCells, [], AvailableGoalZones).

availableGoalCellsToGoalZones_rec([], _, AvailableGoalZones, AvailableGoalZones).
availableGoalCellsToGoalZones_rec([goalCell(X1, Y1)|Rest], CheckedCells, FoundZones, GoalZones) :-
	member(goalCell(X2, Y2), CheckedCells), 
	distanceBetweenPoints_Euclidian(X1, Y1, X2, Y2, Dist),
	Dist =< 5,
	!,
	availableGoalCellsToGoalZones_rec(Rest, [goalCell(X1, Y1)|CheckedCells], FoundZones, GoalZones).
	
availableGoalCellsToGoalZones_rec([goalCell(X1, Y1)|Rest], CheckedCells, FoundZones, GoalZones) :-
	availableGoalCellsToGoalZones_rec(Rest, [goalCell(X1, Y1)|CheckedCells], [goalCell(X1, Y1)|FoundZones], GoalZones).

	
% Rebuild the answers list to the following structure:
% [
%  (goal1, [(agent1, goalcell, [(b0, ...), (b1, ...), ...]),
%	    (agent2, goalcell, [(b0, ...), (b1, ...), ...])]
%  ),
%  (goal2, [...
%           ...]
%  )
% ]	
reorderAnswers(Answers, ReorderedAnswers) :-
	findall(ReorderedAnswers,
		(
			nth0(I, Answers, [Answer, Agent]),  nth0(J, Answer, [GoalCell, List]), 
			length(Answers, NrOfAgents), length(Answer, NrOfGoalCells),
			
			nth0(J, ReorderedAnswers, Agents), nth0(I, Agents, (Agent, GoalCell, List)),
			length(ReorderedAnswers, NrOfGoalCells), length(Agents, NrOfAgents)
		),
		List),
	maplist(=(ReorderedAnswers), List).
	
	
getAllSavedResourceReplies(Task, Replies) :-
	findall(Reply, 	
		(
			savedResourceReply(SavedReply), 
			SavedReply = [Task, Resources, Agent],
			not(occupied(_, Agent)),
			Reply = [Resources, Agent]
		),
		Replies).
		
		
% Heuristic ranking of tasks
getNewTasksRanked(SortedTasks) :-
	findall(task(Name, Deadline, Reward, Requirements), 
		(
			task(Name, Deadline, Reward, Requirements),
			not(taskTaken(_, Name)),
			not(checkedTask(Name))
		), Tasks),
	rankTasks(Tasks, RankedTasks),
	reverseSort(RankedTasks, SortedTasks).
	

findPlan(Task, Answers, TaskPlan) :-
	Answers \= [],
	task(_, Deadline, _, _) = Task,
	reorderAnswers(Answers, ReorderedAnswers),
	
	% Only check a single task and find best assignment
	bestAssignment(Task, ReorderedAnswers, Assignment, ETA),
	Deadline > ETA,
	
	% Build the plan
	buildTaskPlanFromAssignments(Task, Assignment, ETA, TaskPlan).
	
	
buildTaskPlanFromAssignments(Task, Assignments, ETA, TaskPlan) :-
	% Find the task and reqiurements
	task(TaskName, _, _, Requirements) = Task,
	
	% Match assignments to requirements
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


rankTasks(Tasks, RankedTasks) :-
	findall(RankedTask,
		(member(Task, Tasks), rankTask(Task, RankedTask)),
		RankedTasks).
	
rankTask(task(Name, Deadline, Reward, Requirements), RankedTask) :-
	step(CurrentStep),
	RemainingSteps is Deadline - CurrentStep,
	Rank is RemainingSteps + Reward,
	RankedTask = [Rank, task(Name, Deadline, Reward, Requirements)].
	
	

% Recursively find best assignment - check each goalcell
bestAssignment(Task, AnswersToAllGoalZones, BestAssignment, BestETA) :-
	task(TaskName, Deadline, _, Requirements) = Task,
	
	% Extract the blocks from the requirements - make each agent deliver 1 blocktype
	requirementsByBlockType(Requirements, ReqByBlockType),
	
	% Find assignment to each goal - give each goalcell a score
	findall(Assignment, 
		( 
		   member(AnswersToGoalzone, AnswersToAllGoalZones),
		   possibleAssignment(TaskName, Deadline, AnswersToGoalzone, ReqByBlockType, Assignment)
		),
		PossibleAssignments),
	
	% Sort the assignments
	keysort(PossibleAssignments, [BestETA-BestAssignment|_]).



possibleAssignment(TaskName, Deadline, AnswersToGoalZone, Requirements, Assignment) :-
	% First, ensure we have enough available agents
	length(AnswersToGoalZone, NrOfAnswers),
	length(Requirements, NrOfRequirements),
	NrOfAnswers >= NrOfRequirements,
	
	% Find all possible Agent Assignment
	findall(MaxDistance-Match,
			( 
				matchAgentsToRequirements(TaskName, AnswersToGoalZone, Requirements, [], [], Match),
				findall(Distance, member((_, _, Distance, _, _, _), Match), Distances),
				max_list(Distances, MaxDistance)
			),
			Matches),
	
	% Find the assignment with the best score
	min_member(BestMatchDistance-BestMatch, Matches),
	
	% Attach the score of the match
	step(Step),
	ETA is Step + (2*BestMatchDistance),
	ETA =< Deadline,
	Assignment = ETA-BestMatch.
		
		

matchAgentsToRequirements(_, [], [_|_], _, _, []).

matchAgentsToRequirements(_, _, [], SubmitAgentPlan, OtherAgentPlans, Matches) :-
	SubmitAgentPlan \= [],
	append(SubmitAgentPlan, OtherAgentPlans, Matches).
	
matchAgentsToRequirements(TaskName, Agents, [(BlockType, ReqQty, Positions)|OtherRequirements], [], OtherAgentPlans, Matches) :-
	% Only assign submit-agents to the closest blocks
	member((X, Y), Positions),
	translate(_, 0, 0, X, Y),
	
	% Find an agent from answers
	select((Agent, GoalCell, BlockList), Agents, AgentsRest),
	member(AvailableBlock, BlockList),
	AvailableBlock = (BlockType, _, [TotalDist|Stops], BlockQty),
	ReqQty =< BlockQty,
	append(Stops, [GoalCell], Path),
	SubmitAgentPlan = [(Agent, TaskName, TotalDist, BlockType, ReqQty, Path, Positions)],
	
	% Add the agent to the plan
	matchAgentsToRequirements(TaskName, AgentsRest, OtherRequirements, SubmitAgentPlan, OtherAgentPlans, Matches).

matchAgentsToRequirements(TaskName, Agents, [(BlockType, ReqQty, Positions)|OtherRequirements], SubmitAgentPlan, OtherAgentPlans, Matches) :-
	% Find an agent from answers
	select((Agent, GoalCell, BlockList), Agents, AgentsRest),
	member(AvailableBlock, BlockList),
	AvailableBlock = (BlockType, [Dist|Stops], _, BlockQty),
	ReqQty =< BlockQty,
	append(Stops, [GoalCell], Path),
	AgentPlan = (Agent, TaskName, Dist, BlockType, ReqQty, Path, Positions),
	
	% Add the agent to the plan
	matchAgentsToRequirements(TaskName, AgentsRest, OtherRequirements, SubmitAgentPlan, [AgentPlan|OtherAgentPlans], Matches).
	
	
requirementsByBlockType(Requirements, FilteredRequirements) :-
	% First, find all the inner blocks
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
	
	% Order the other requirements by block type
	findall(BlockType, member(req(_, _, BlockType), OtherRequirements), BlockTypes),
	setof((BlockType, Qty), (member(BlockType, BlockTypes), count(BlockTypes, BlockType, Qty)), Blocks),
	findall((BlockType, Qty, Positions),
		(
			member((BlockType, Qty), Blocks),
			findall((X, Y), member(req(X, Y, BlockType), OtherRequirements), Positions)
		),
		OtherRequirementsByBlockType),
	
	% Append the requirements
	append(SubmitAgentRequirements, OtherRequirementsByBlockType, FilteredRequirements).	
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%% PLANNING - OTHER AGENTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
getResourcesForTask(Task, GoalCells, Resources) :-
	% For each given goalCell, find fastest ways to get each block type
	findall(ClosestResources,
		( member(GoalCell, GoalCells), closestResourcesToGoalCellForTask(GoalCell, Task, ClosestResources) ),
		Resources).
		

closestResourcesToGoalCellForTask(GoalCell, Task, ClosestResourcesToGoalCell) :-
	task(_, Deadline, _, Requirements) = Task,
	step(Step),
	MaxDist is 0.5 * (Deadline - Step),
	
	% Find all known block types
	findall(BlockType, member(req(_, _, BlockType), Requirements), RepeatingBlockTypes),
	sort(RepeatingBlockTypes, BlockTypes),

	% Find closest resource of each blocktype
	findall(ClosestResource,
		(member(BlockType, BlockTypes), closestBlockResourceToGoalCell(BlockType, GoalCell, MaxDist, ClosestResource)),
		ClosestResources),
		
	ClosestResourcesToGoalCell = [GoalCell, ClosestResources].
		
		
closestBlockResourceToGoalCell(BlockType, GoalCell, MaxDist, ClosestResource) :-
	% Check if we have any blocks of that type attached
	findall(_, attachedToMe(_, _, block, BlockType), AttachedBlocks), length(AttachedBlocks, NrOfAttachedBlocks),
	
	% Find closest resource
	(NrOfAttachedBlocks > 0 -> 
	      closestResourcesWithAttachedBlocks(NrOfAttachedBlocks, BlockType, GoalCell, MaxDist, ClosestResource)
	      ;
	      closestResourcesWithoutAttachedBlocks(BlockType, GoalCell, MaxDist, ClosestResource)
	).


closestResourcesWithAttachedBlocks(NrOfAttachedBlocks, BlockType, GoalCell, MaxDist, ClosestResource) :-
	goalCell(X, Y) = GoalCell, myPosition(MyX, MyY), 
	distanceBetweenPoints_Manhattan(MyX, MyY, X, Y, DistToGoal),
	DistToGoal < MaxDist, 
	Qty = NrOfAttachedBlocks,
	closestTaskboardToGoal(goalCell(X, Y), Taskboard, DistWithTaskboard),
	(DistWithTaskboard < MaxDist ->
		ClosestResource = (BlockType, [DistToGoal], [DistWithTaskboard, Taskboard], Qty)
		;
		ClosestResource = (BlockType, [DistToGoal], [], Qty)
	).


closestResourcesWithoutAttachedBlocks(BlockType, GoalCell, MaxDist, ClosestResource) :-
	Qty is 2,
        closestDispenserToGoal(GoalCell, BlockType, Dispenser, DistWithDispenser),
        DistWithDispenser < MaxDist,
        closestDispenserAndTaskboardToGoal(GoalCell, BlockType, ClosestElement, NextElement, TotalDist),
        (TotalDist < MaxDist ->
        	ClosestResource = (BlockType, [DistWithDispenser, Dispenser], [TotalDist, ClosestElement, NextElement], Qty)
        	;
        	ClosestResource = (BlockType, [DistWithDispenser, Dispenser], [], Qty)
        ).
        
        	
         

closestTaskboardToGoal(goalCell(X, Y), Taskboard, ShortestDist) :-
	myPosition(MyX, MyY), 
	findall([Distance, taskboard(Xt, Yt)],
	                 (
	                 	taskboard(Xt, Yt), 
	                 	distanceBetweenPoints_Manhattan(MyX, MyY, Xt, Yt, DistanceToTaskboard),
	                 	distanceBetweenPoints_Manhattan(Xt, Yt, X, Y, DistanceFromTaskboard),
	                 	Distance is DistanceToTaskboard + DistanceFromTaskboard
	                 ),
	                 Taskboards),
	         sort(Taskboards, SortedTaskboards), 
	         SortedTaskboards = [[ShortestDist, Taskboard]|_].
	         

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


closestDispenserAndTaskboardToGoal(goalCell(X, Y), BlockType, ClosestElement, NextElement, TotalDist) :-
	myPosition(MyX, MyY),
	
	findall(dispenser(Xd, Yd, BlockType), dispenser(Xd, Yd, BlockType), Dispensers),
	findall(taskboard(Xt, Yt), taskboard(Xt, Yt), Taskboards),
	
	% find shortest combination
	findall([Dist, Elem1, Elem2],
		(
			(	
				member(taskboard(Xt, Yt), Taskboards),
				member(dispenser(Xd, Yd, BlockType), Dispensers),
				
				(
					( Elem1 = taskboard(Xt, Yt),
					  Elem2 = dispenser(Xd, Yd, BlockType),
					  
					  distanceBetweenPoints_Manhattan(MyX, MyY, Xt, Yt, Dist1),
					  distanceBetweenPoints_Manhattan(Xt, Yt, Xd, Yd, Dist2),
					  distanceBetweenPoints_Manhattan(Xd, Yd, X, Y, Dist3),
					  Dist is Dist1 + Dist2 + Dist3
					) ;
					( 
					  Elem1 = dispenser(Xd, Yd, BlockType),
					  Elem2 = taskboard(Xt, Yt),
					  
					  distanceBetweenPoints_Manhattan(MyX, MyY, Xd, Yd, Dist1),
					  distanceBetweenPoints_Manhattan(Xd, Yd, Xt, Yt, Dist2),
					  distanceBetweenPoints_Manhattan(Xt, Yt, X, Y, Dist3),
					  Dist is Dist1 + Dist2 + Dist3
					)
				)
			)
		),
		AllCombinations),
		
	sort(AllCombinations,  [[TotalDist, ClosestElement, NextElement]|_]).	         
	        

nextTaskMaster(ConnectedAgents, NextTaskMaster) :-
	name(MyName),
	list_to_ord_set(ConnectedAgents, ConnectedAgents_Ordered),
	ord_add_element(ConnectedAgents_Ordered, MyName, Network),
	[NextTaskMaster|_] = Network.


findCommonTaskMaster(ConnectionUpdateList, NewConnections, CommonTaskMaster) :-
	% Should only contain a single taskmaster
	findall(TaskMaster, taskMaster(TaskMaster), MyTaskMasters),
	
	% Find taskmasters of connections
	findall(TaskMaster, 
		(
		 member((Agent, _, _), NewConnections),
		 member((Agent, _, _, _, TaskMasterList), ConnectionUpdateList),
		 member(TaskMaster, TaskMasterList)
		),
		ConnTaskMasters),
	
	% Order the taskmasters and add my own
	list_to_ord_set(ConnTaskMasters, Ord_ConnTaskMasters),
	ord_union(Ord_ConnTaskMasters, MyTaskMasters, TaskMasters),
	
	CommonTaskMaster = TaskMasters.
	


taskMastersOrdered(TaskMasters_Ordered) :-
	findall(TaskMaster, taskMaster(TaskMaster), TaskMasters),
	list_to_ord_set(TaskMasters, TaskMasters_Ordered).





 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILITY FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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




















% If we move closer, it must be constructive.
constructiveMove(_, _, _) :-
	thing(Xc, Yc, marker, clear); thing(Xc, Yc, marker, ci).

constructiveMove(GoalXr, GoalYr, Dir)  :-
	translate(Dir, 0, 0, Xr, Yr),
	not(recentlyVisited(Xr, Yr)),
	distanceBetweenPoints_Manhattan(0, 0, GoalXr, GoalYr, MyDist),
	distanceBetweenPoints_Manhattan(Xr, Yr, GoalXr, GoalYr, DistAfterMove),
	DistAfterMove < MyDist,
	!.

% If we move along an obstacle, then it is constructive
constructiveMove(GoalXr, GoalYr, Dir)  :-
	translate(Dir, 0, 0, Xr, Yr),
	not(recentlyVisited(Xr, Yr)),
	distanceBetweenPoints_Manhattan(0, 0, GoalXr, GoalYr, MyDist),
	impassableObjectClose(0, 0, Xo, Yo),
	impassableObjectClose(Xr, Yr, Xo, Yo),
	distanceBetweenPoints_Manhattan(Xo, Yo, GoalXr, GoalYr, ObjectDist),
	ObjectDist < MyDist,
	!.

recentlyVisited(Xr, Yr) :-
	step(CurrentStep),
	relativeToAbsolutePositionOfCoordinates(Xr, Yr, X, Y),
	visited(X, Y, Step),
	2 < CurrentStep - Step.	

impassableObjectClose(X, Y, Xobject, Yobject) :-
	surroundingCells(X, Y, SurroundingCells),
	member((Xobject, Yobject), SurroundingCells), 
	impassable(Xobject, Yobject).

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
	

% astar(+X,+Y,-Path, -Actions) - Starts A-star, Path will include a complete path to (GoalX,GoalY), Actions include what actions the agent must perform. 
% The astar will only succeed if we find a goalpoint closer than we started
astarNoClear(GoalXr,GoalYr,Path, Actions) :- 
    empty_heap(Frontier),
    getAttachments(Attachments),
    energy(Level),
    expandFrontierNoClear(0, 0, 0, 0, Level, [], [], Attachments, [], GoalXr, GoalYr, Frontier, [], ExpandedFrontier),
    astarRecursiveNoClear(GoalXr, GoalYr, ExpandedFrontier, [state((0,0), Attachments, [])], [(FinalX, FinalY)|RevPath], RevActions),!,
    % Ensure we ended up closer than we started
    distanceBetweenPoints_Euclidian(0, 0, GoalXr, GoalYr, StartDist),
    distanceBetweenPoints_Euclidian(FinalX, FinalY, GoalXr, GoalYr, ResultDist),
    ResultDist < StartDist,
    % Reverse the found path and actions
    reverse([(FinalX, FinalY)|RevPath], Path_relative),
    maplist(relativeToAbsolutePositionOfPoints, Path_relative, Path),
    reverse(RevActions, Actions).
    
    
   
clearGuard :-
	(obstacle(X, Y) ; ( thing(X, Y, block, _), not(attached(X, Y))) ).

astarClear(GoalXr, GoalYr, Path, Actions) :- 
    empty_heap(Frontier),
    getAttachments(Attachments),
    energy(Level),
    validClearActions(0,0, 0, 0, Level, [], [], Attachments, [], GoalXr, GoalYr, [], Frontier, ExpandedFrontier),
    astarRecursiveClear(GoalXr, GoalYr, ExpandedFrontier, [state((0,0), Attachments, [])], [(FinalX, FinalY)|RevPath], RevActions),!,
    % Ensure we ended up closer than we started
    distanceBetweenPoints_Euclidian(0, 0, GoalXr, GoalYr, StartDist),
    distanceBetweenPoints_Euclidian(FinalX, FinalY, GoalXr, GoalYr, ResultDist),
    ResultDist < StartDist,
    % Reverse the found path and actions
    reverse([(FinalX, FinalY)|RevPath], Path_relative),
    maplist(relativeToAbsolutePositionOfPoints, Path_relative, Path),
    reverse(RevActions, Actions).
   
   
   
    
% astarRecursive(+X,+Y,+Frontier, +ExpandedStates, -Path, -Actions) - used to recursively search for the next preferable step. 
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


% expandFrontier(+X,+Y, +ParentStepCost, +ParentRotationLimit, +ParentEnergy, +ParentPath, +ParentActions, +ParentAttachments, +ParentClearedCells, +GoalX, +GoalY, +Frontier, +ExpandedStates, -ExpandedFrontier) - used to expand frontier in A-star.
expandFrontierNoClear(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, ParentPath, ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, Frontier, ExpandedStates, ExpandedFrontier):-
    validMoveActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, Frontier, MoveFrontier),!,
    validRotateActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, MoveFrontier, ExpandedFrontier),!.
   
expandFrontierClear(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, ParentPath, ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, Frontier, ExpandedStates, ExpandedFrontier):-
    validMoveActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, Frontier, MoveFrontier),!,
    validRotateActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, MoveFrontier, RotateFrontier),!,
    validClearActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, [(X,Y)|ParentPath], ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, RotateFrontier, ExpandedFrontier),!.
    
    
% Find all valid move actions.
% validMoveActions(+X,+Y, +ParentStepCost, +ParentRotationLimit, +ParentEnergy, +Path, +ParentActions, +Attachments, +ParentClearedCells, +GoalX, +GoalY,  +Frontier, -ExpandedFrontier)
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
	 	

% Find all valid rotate actions.
% validRotateActions(+X,+Y, +ParentStepCost, +ParentRotationLimit, +ParentEnergy, +Path, +ParentActions, +ParentAttachments, +ParentClearedCells, +GoalX, +GoalY, +ExpandedStates, +Frontier, -ExpandedFrontier)
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
            	rotateAttachmentsAttached(R,ParentAttachments, Attachments),
            	not(member(state((X,Y), Attachments, ParentClearedCells),ExpandedStates))),
            PassableStates),
    insertListInHeap(PassableStates, Frontier, ExpandedFrontier).
    
% Find all valid clear actions.
% validClearActions(+X,+Y, +ParentStepCost, +ParentRotationLimit, +ParentEnergy, +Path, +ParentActions, +ParentAttachments, +ParentClearedCells, +GoalX, +GoalY, +ExpandedStates, +Frontier, -ExpandedFrontier)
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




% Evaluate a given position
heuristic(Xr, Yr, GoalXr, GoalYr, StepCost, Heuristic) :-
    distMan(Xr, Yr, GoalXr, GoalYr, Dist),
    Heuristic is Dist + StepCost.



% Insert list in heap
% insertListInHeap(+List, +Heap, -ExpandedHeap)
insertListInHeap(List, Heap, ExpandedHeap):-  
	list_to_heap(List, ListHeap),
    	merge_heaps(Heap, ListHeap, ExpandedHeap).


% get cleared cells after a clear action
% clearedCells(+CellX, +CellY, -ClearedCells)
clearedCells(CellX, CellY, ClearedCells):-
	cellsInRange(CellX, CellY, 1, Cells), 
	findall((X, Y),
		(member((X, Y), Cells),
		(
			thing(X, Y, block, _);
			obstacle(X, Y)
		)),
		ClearedCells).

% Used to check if an agent has enough energy to perform a clear action
% checkEnergy(+CurrentEnergy, -UpdatedEnergy)       
checkEnergy(CurrentEnergy, UpdatedEnergy):-
    clearSteps(ClearSteps),
    clearEnergyCost(ClearCost), 
    TotalEnergyCost = ClearSteps*ClearCost,
    CurrentEnergy >= TotalEnergyCost,
    UpdatedEnergy is CurrentEnergy - TotalEnergyCost.

% Used to update energy level in internal state in astar. 
% updateEnergy(+CurrentEnergy, -UpdatedEnergy)
updateEnergy(CurrentEnergy, UpdatedEnergy):- 
	((maxEnergy(MaxLevel), CurrentEnergy < MaxLevel) -> UpdatedEnergy is CurrentEnergy +1; UpdatedEnergy is CurrentEnergy).

% Used to check if an obstacle is in one of the surrounding cells. 
%obstacleCloseToAgent(+X,+Y)
obstacleClose(X,Y):-
	surroundingCells(X,Y, SurroundingCells),
	member((RelativeAdjacentX,RelativeAdjacentY), SurroundingCells), 
	obstacle(RelativeAdjacentX,RelativeAdjacentY).
 
 % Used to find diagonal and nondiagonal adjacent cells in relative coordinates.
% surroundingCells(+X,+Y,-Cells)
surroundingCells(X,Y,Cells):-
	findall((RelativeAdjacentX,RelativeAdjacentY),
			(between(-1,1,LocalAdjacentX), 
			between(-1,1,LocalAdjacentY),
			not((LocalAdjacentX = 0, LocalAdjacentY = 0)), 
			RelativeAdjacentX is X+LocalAdjacentX, 
			RelativeAdjacentY is Y+LocalAdjacentY),
		Cells).


%astarBlockedRotation(-MyX, -MyY, -R, -Attachments, -ClearedCells) - used to check if an rotation is blocked
astarBlockedRotation(MyX, MyY, R, Attachments, ClearedCells) :- 
	member(attachedToMe(X_att, Y_att, _, _),Attachments),
	rotation90(R, X_att, Y_att, Xr, Yr),
	X is MyX + Xr,
	Y is MyY + Yr,
	astarImpassable(X, Y, MyX, MyY),
	not(member((X, Y), ClearedCells)).
	

% Is direction blocked - coordinates relative to origin
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
	
%astarImpassable(-X, -Y, -MyX, -MyY) - used to check if it's impossible to move to (X,Y) from (MyX,MyY).
astarImpassable(X, Y,_,_) :- obstacle(X, Y).
astarImpassable(X, Y,_,_) :- thing(X, Y, entity, _).
astarImpassable(X, Y, MyX, MyY) :- thing(X, Y, block, BlockType), AttahcmentX is X-MyX, AttahcmentY is Y-MyY, not(attachedToMe(AttahcmentX, AttahcmentY, block, BlockType)).


% Used to find all cells in Range from (X,Y)	
% cellsInRange(+X,+Y, +Range, -Cells)	
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

getAttachments(Attachments) :-
	findall(attachedToMe(X, Y, block, BlockType), attachedToMe(X, Y, block, BlockType), Attachments).


rotateAttachmentsAttached(R, Attachments, AttachmentsRotated) :-
	findall(attachedToMe(Xr, Yr, block, BlockType), 
		(
			member(attachedToMe(X, Y, block, BlockType), Attachments), 
			rotation90(R, X, Y, Xr, Yr)
		), 
		AttachmentsRotated).
		



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

%!  add_to_heap(+Heap0, +Priority, ?Key, -Heap) is semidet.
%
%   Adds Key with priority Priority  to   Heap0,  constructing a new
%   heap in Heap.

add_to_heap(heap(Q0,M),P,X,heap(Q1,N)) :-
    meld(Q0,t(X,P,[]),Q1),
    N is M+1.

%!  delete_from_heap(+Heap0, -Priority, +Key, -Heap) is semidet.
%
%   Deletes Key from Heap0, leaving its priority in Priority and the
%   resulting data structure in Heap.   Fails if Key is not found in
%   Heap0.
%
%   @bug This predicate is extremely inefficient and exists only for
%        SICStus compatibility.

delete_from_heap(Q0,P,X,Q) :-
    get_from_heap(Q0,P,X,Q),
    !.
delete_from_heap(Q0,Px,X,Q) :-
    get_from_heap(Q0,Py,Y,Q1),
    delete_from_heap(Q1,Px,X,Q2),
    add_to_heap(Q2,Py,Y,Q).

%!  empty_heap(?Heap) is semidet.
%
%   True if Heap is an empty heap. Complexity: constant.

empty_heap(heap(nil,0)).

%!  singleton_heap(?Heap, ?Priority, ?Key) is semidet.
%
%   True if Heap is a heap with the single element Priority-Key.
%
%   Complexity: constant.

singleton_heap(heap(t(X,P,[]), 1), P, X).

%!  get_from_heap(?Heap0, ?Priority, ?Key, -Heap) is semidet.
%
%   Retrieves the minimum-priority  pair   Priority-Key  from Heap0.
%   Heap is Heap0 with that pair removed.   Complexity:  logarithmic
%   (amortized), linear in the worst case.

get_from_heap(heap(t(X,P,Sub),M), P, X, heap(Q,N)) :-
    pairing(Sub,Q),
    N is M-1.

%!  heap_size(+Heap, -Size:int) is det.
%
%   Determines the number of elements in Heap. Complexity: constant.

heap_size(heap(_,N),N).

%!  heap_to_list(+Heap, -List:list) is det.
%
%   Constructs a list List  of   Priority-Element  terms, ordered by
%   (ascending) priority. Complexity: $O(n \log n)$.

heap_to_list(Q,L) :-
    to_list(Q,L).
to_list(heap(nil,0),[]) :- !.
to_list(Q0,[P-X|Xs]) :-
    get_from_heap(Q0,P,X,Q),
    heap_to_list(Q,Xs).

%!  is_heap(+X) is semidet.
%
%   Returns true if X is a heap.  Validates the consistency of the
%   entire heap. Complexity: linear.

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

% True iff 1st arg is a pairing heap with min key @=< 2nd arg,
% where min key of nil is logically @> any term.
is_pairing_heap(V, _) :-
    var(V),
    !,
    fail.
is_pairing_heap(nil, _).
is_pairing_heap(t(_,P,Sub), MinP) :-
    MinP @=< P,
    are_pairing_heaps(Sub, P).

% True iff 1st arg is a list of pairing heaps, each with min key @=< 2nd arg.
are_pairing_heaps(V, _) :-
    var(V),
    !,
    fail.
are_pairing_heaps([], _).
are_pairing_heaps([Q|Qs], MinP) :-
    is_pairing_heap(Q, MinP),
    are_pairing_heaps(Qs, MinP).

%!  list_to_heap(+List:list, -Heap) is det.
%
%   If List is a list of  Priority-Element  terms, constructs a heap
%   out of List. Complexity: linear.

list_to_heap(Xs,Q) :-
    empty_heap(Empty),
    list_to_heap(Xs,Empty,Q).

list_to_heap([],Q,Q).
list_to_heap([P-X|Xs],Q0,Q) :-
    add_to_heap(Q0,P,X,Q1),
    list_to_heap(Xs,Q1,Q).

%!  min_of_heap(+Heap, ?Priority, ?Key) is semidet.
%
%   Unifies Key with  the  minimum-priority   element  of  Heap  and
%   Priority with its priority value. Complexity: constant.

min_of_heap(heap(t(X,P,_),_), P, X).

%!  min_of_heap(+Heap, ?Priority1, ?Key1, ?Priority2, ?Key2) is semidet.
%
%   Gets the two minimum-priority elements from Heap. Complexity: logarithmic
%   (amortized).
%
%   Do not use this predicate; it exists for compatibility with earlier
%   implementations of this library and the SICStus counterpart. It performs
%   a linear amount of work in the worst case that a following get_from_heap
%   has to re-do.

min_of_heap(Q,Px,X,Py,Y) :-
    get_from_heap(Q,Px,X,Q0),
    min_of_heap(Q0,Py,Y).

%!  merge_heaps(+Heap0, +Heap1, -Heap) is det.
%
%   Merge the two heaps Heap0 and Heap1 in Heap. Complexity: constant.

merge_heaps(heap(L,K),heap(R,M),heap(Q,N)) :-
    meld(L,R,Q),
    N is K+M.


% Merge two pairing heaps according to the pairing heap definition.
meld(nil,Q,Q) :- !.
meld(Q,nil,Q) :- !.
meld(L,R,Q) :-
    L = t(X,Px,SubL),
    R = t(Y,Py,SubR),
    (   Px @< Py
    ->  Q = t(X,Px,[R|SubL])
    ;   Q = t(Y,Py,[L|SubR])
    ).

% "Pair up" (recursively meld) a list of pairing heaps.
pairing([], nil).
pairing([Q], Q) :- !.
pairing([Q0,Q1|Qs], Q) :-
    meld(Q0, Q1, Q2),
    pairing(Qs, Q3),
    meld(Q2, Q3, Q).
	
	

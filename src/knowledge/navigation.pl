% Translate coordinates in one of the four main directions.
%% 	translate(Direction, X, Y, X_Translated, Y_Translated)
translate(n, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 - 1.
translate(s, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 + 1.
translate(e, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 + 1.
translate(w, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 - 1.

% Calculates the resulting position of an object after either 90 0r 180 degree rotation around the agent.
%% 	rotationXX(Rotation, StartX, StartY, ResultX, ResultY)
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

% Infer the direction of a relative position.
direction(Xr, Yr, D) :-
	translate(D, 0, 0, Xr, Yr).

% Returns true if (X, Y) and (Xa, Ya) are adjacent.
adjacent(X, Y, Xa, Ya) :-
	translate(_, X, Y, Xa, Ya).
	
% Infer angle and rotation to rotate the relative position (Xr, Yr) towards (Xt, Yt)
rotation(R, Xr, Yr, Xt, Yt, Angle) :- rotation90(R, Xr, Yr, Xt, Yt), Angle = 90.
rotation(R, Xr, Yr, Xt, Yt, Angle) :- rotation180(R, Xr, Yr, Xt, Yt), Angle = 180.

% Return the details of an attached object at (Xr, Yr).
attachedToMe(Xr, Yr, Type, Details) :-
	translate(_, 0, 0, Xr, Yr),
	attachedToMe(Xr, Yr),
	thing(Xr, Yr, Type, Details),
	(Type = block; Type = entity).	

% TODO - check usage and clean!
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
	

% Returns true if the position (Xr, Yr) contains an impassable object.	
impassable(X, Y) :- obstacle(X, Y).
impassable(X, Y) :- thing(X, Y, entity, _).
impassable(X, Y) :- thing(X, Y, block, BlockType), not(attachedToMe(X, Y, block, BlockType)).

% Check whether a direction is blocked
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


% Get the resulting positions of the agent's attachments after performing a rotation.
rotateAttachments(R, Attachments, AttachmentsRotated) :-
	findall(attachedToMe(Xr, Yr, block, BlockType), 
		(
			member(attachedToMe(X, Y, block, BlockType), Attachments), 
			rotation90(R, X, Y, Xr, Yr)
		), 
		AttachmentsRotated).	
	

% True if the agent can attach a block at the relative position (Xr, Yr).
% An agent can only attach two blocks on opposite sides.
availableAttachmentSpot(Xr, Yr) :-
	getAttachments(Attachments),
		
	% If nothing attached, then any spot is fine
	(
		Attachments = [] ->
			member((Xr, Yr), [(1, 0), (0, 1), (-1, 0), (0, -1)])
			;
			% Otherwise, it should be opposite the block already attached
			(
				[attachedToMe(Xa, Ya, _, _)] = Attachments,
				rotation180(cw, Xa, Ya, Xr, Yr)
			)
	).


% Return all things attached to the agent.
getAttachments(Attachments) :-
	findall(attachedToMe(X, Y, block, BlockType), attachedToMe(X, Y, block, BlockType), Attachments).
	
% True if the agent must rotate in order to attach an object at the relative location (Xr, Yr).
rotationRequiredToAttach(Xr, Yr) :-
	not(availableAttachmentSpot(Xr, Yr)), availableAttachmentSpot(_, _).
			
			
% Find a possible rotation
possibleRotationToAttach(Xr, Yr, R) :-
	availableAttachmentSpot(Xa, Ya),
	rotation(R, Xa, Ya, Xr, Yr, Angle),
	% check if we have to rotate 90 or 180 degrees.
	(
		Angle = 90 ->
			not(blockedRotation(R, 90));
			(not(blockedRotation(R, 90)), not(blockedRotation(R, 180)))
	).
	

% Generate pseudorandom direction
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

% Find the epicenter of a clear event
epicenter(X, Y) :-
	findall((Xc, Yc), (thing(Xc, Yc, marker, clear); thing(Xc, Yc, marker, ci)), StrikeZone),
	StrikeZone \= [],
	outerPoints(StrikeZone, [(WestX, _), (_, NorthY), (EastX, _), (_, SouthY)]),
	X is (WestX + EastX)/2,
	Y is (NorthY + SouthY)/2.

% Find the outermost points (that defines the perimeter) from a given list of points (X, Y).
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

% Find the best action in order to explore the map.
% Action is one of:
%	- move(Direction)
%	- rotate(Rotation)
%	- clear(X, Y)	
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


% Find the best action in order to move towards the location (Xr, Yr).
% Action is one of:
%	- move(Direction)
%	- rotate(Rotation)
%	- clear(X, Y)
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


% Find the closest block/dispenser in vision without any nearby agents
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
% Translate coordinates in one of the four main directions.
%% 	translate(Direction, X, Y, X_Translated, Y_Translated)
translate(n, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 - 1.
translate(s, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 + 1.
translate(e, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 + 1.
translate(w, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 - 1.

% Infer the direction of a relative position.
direction(Xr, Yr, D) :-
	translate(D, 0, 0, Xr, Yr).

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

% Infer angle and rotation to rotate the relative position (Xr, Yr) towards (Xt, Yt)
rotation(R, Xr, Yr, Xt, Yt, Angle) :- rotation90(R, Xr, Yr, Xt, Yt), Angle = 90.
rotation(R, Xr, Yr, Xt, Yt, Angle) :- rotation180(R, Xr, Yr, Xt, Yt), Angle = 180.

% Returns true if (X, Y) and (Xa, Ya) are adjacent.
adjacent(X, Y, Xa, Ya) :-
	translate(_, X, Y, Xa, Ya).

% Return the details of an attached object at (Xr, Yr).
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
	
% Return all things attached to the agent.
getAttachments(Attachments) :-
	findall(attachedToMe(X, Y, block, BlockType), _, Attachments).
	
	

% True if the agent can attach a block at the relative position (Xr, Yr).
% An agent can only attach two blocks on opposite sides.
availableAttachmentSpot(Xr, Yr) :- 
	% Find all attachments
	findall(attachedToMe(Xa, Ya, Type, Details),
		attachedToMe(Xa, Ya, Type, Details),
		Attachments),
		
	% If nothing attached, then any spot is fine
	(
		Attachments = [] ->
			member((Xr, Yr), [(1, 0), (0, 1), (-1, 0), (0, -1)])
			;
			% Otherwise, it should be opposite the block already attached
			(
				[attachedToMe(Xa, Ya, Type, Details)] = Attachments,
				rotation180(cw, Xa, Ya, Xr, Yr)
			)
	).

	
% True if the agent must rotate in order to attach an object at the relative location (Xr, Yr).
rotationRequiredToAttach(Xr, Yr) :-
	not(availableAttachmentSpot(Xr, Yr)), availableAttachmentSpot(_, _).
			
			
% Find a possible rotation
possibleRotation(Xr, Yr, R) :-
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

% TODO
%actionScore(move) :-
%actionScore(rotate) :-
%actionScore(clear) :-

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

distanceScore(D, X_target, Y_target, Score) :-
	myPosition(MyX, MyY),
	distanceBetweenPoints_Manhattan(MyX, MyY, X_target, Y_target, CurrentDistance),
	translate(D, MyX, MyY, X_new, Y_new),
	distanceBetweenPoints_Manhattan(X_new, Y_new, X_target, Y_target, NewDistance),
	Score is CurrentDistance - NewDistance.

safeScore(D, Score) :-
	translate(D, 0, 0, Xr, Yr),
	(thing(Xr, Yr, marker, clear); thing(Xr, Yr, marker, ci)) ->
		(epicenter(Xe, Ye), distMan(Xr, Yr, Xe, Ye, Dist), Score is 10*Dist);
		(Score = 0).

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
	
validDirectionAfterRotation(D) :-
	anyDirection(D),
	blocked(D),
	attachedToMe(_, _, _, _),
	blockedForMyAttachments(D),
	not(blockedRotation(R, 90)),
	not(blockedAfterRotation(R, D)).

validClearingDirection(D) :-
	anyDirection(D), blocked(D), translate(D, 0, 0, X, Y), thing(X, Y, obstacle, _).
	
moveDirection(D, Penalty) :-
	(validDirection(D), Penalty = 0) ; 
	(validDirectionAfterRotation(D), Penalty = 0.1) ; 
	((energy(E), clearEnergyCost(C), E > C) -> (validClearingDirection(D), Penalty = 0.5)).
	
exploreAction(Action) :-
	findall((Score, D),
		 (moveDirection(D, Penalty), exploreScore(D, EScore), disruptScore(D, DScore), safeScore(D, SScore), 
		 	Score is EScore + DScore + SScore - Penalty),
	    	 DirectionValueList),
	reverseSort(DirectionValueList, DirectionValueListSorted),
	DirectionValueListSorted = [(MaxScore, _)|_],
	step(N), Seed is N mod(24), enumDirList(DL, Seed),
	member(Direction, DL), 	member((S, Direction), DirectionValueListSorted), S = MaxScore,
	extractAction(Direction, Action).

	

% Find the best action in order to move towards the location (Xr, Yr).
% Action is one of:
%	- move(Direction)
%	- rotate(Rotation)
%	- clear(X, Y)
goToAction(Xr, Yr, Action) :-
	findall((Score, D), 
		(moveDirection(D, _), 
			exploreScore(D, EScore), distanceScore(D, Xr, Yr, DScore), safeScore(D, SScore), 
			Score is (DScore + EScore + SScore)
		), 
		DirectionValueList),
	reverseSort(DirectionValueList, DirectionValueListSorted),
	DirectionValueListSorted = [(MaxScore, _)|_],
	step(N), Seed is N mod(24), enumDirList(DL, Seed),
	member(Direction, DL), 	member((S, Direction), DirectionValueListSorted), S = MaxScore,
	extractAction(Direction, Action).


extractAction(D, Action) :-
	validDirection(D) -> Action = move(D) ;
	(
		validDirectionAfterRotation(D) ->
			( member(R, [cw, ccw]), not(blockedRotation(R, 90)), Action = rotate(R) ) ;
				
				( clearCoordinatesFromDirection(D, X, Y), Action = clear(X, Y) )
	).
	

clearCoordinatesFromDirection(D, X, Y) :-
	translate(D, 0, 0, X1, Y1),
	(myRole(digger) ->
		(translate(D, X1, Y1, X, Y)) ;
		(X = X1, Y = Y1)
	).

	
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
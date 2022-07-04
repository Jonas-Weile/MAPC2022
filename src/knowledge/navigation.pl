 %%%%%% NAVIGATION %%%%%%

%% Relative navigation
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
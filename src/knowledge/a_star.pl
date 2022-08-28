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

astarClear(GoalXr, GoalYr, Path, Actions) :- 
    empty_heap(Frontier),
    getAttachments(Attachments),
    energy(Level),
    expandFrontierClear(0, 0, 0, 0, Level, [], [], Attachments, [], GoalXr, GoalYr, Frontier, [], ExpandedFrontier),
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
    StepCost is ParentStepCost + 2,
	heuristic(X, Y, GoalX, GoalY, StepCost, Heuristic),
    updateEnergy(ParentEnergy, UpdatedEnergy),
    findall(Heuristic-state((X,Y), StepCost, RotationLimit, UpdatedEnergy, Path, [rotate(R)|ParentActions], Attachments, ParentClearedCells),
            (member(R,[cw,ccw]),
                not(astarBlockedRotation(X, Y, R, ParentAttachments,ParentClearedCells)), 
            	rotateAttachments(R,ParentAttachments, Attachments),
            	not(member(state((X,Y), Attachments, ParentClearedCells),ExpandedStates))),
            PassableStates),
    insertListInHeap(PassableStates, Frontier, ExpandedFrontier).
    
% Find all valid clear actions.
% validClearActions(+X,+Y, +ParentStepCost, +ParentRotationLimit, +ParentEnergy, +Path, +ParentActions, +ParentAttachments, +ParentClearedCells, +GoalX, +GoalY, +ExpandedStates, +Frontier, -ExpandedFrontier)
validClearActions(_,_, _, _, ParentEnergy, _, _, _, _, _, _, _, Frontier, Frontier):-
    not(checkEnergy(ParentEnergy, _)).
    
validClearActions(X,Y, ParentStepCost, ParentRotationLimit, ParentEnergy, Path, ParentActions, ParentAttachments, ParentClearedCells, GoalX, GoalY, ExpandedStates, Frontier, ExpandedFrontier):-
    team(Team),
    checkEnergy(ParentEnergy, EnergyAfterClear),
    updateEnergy(EnergyAfterClear, UpdatedEnergy),
    StepCost is ParentStepCost + 5,
    heuristic(X, Y, GoalX, GoalY, StepCost, Heuristic),
    findall(Heuristic-state((X,Y), StepCost, ParentRotationLimit, UpdatedEnergy, Path, [clear(CellX, CellY)|ParentActions], ParentAttachments, AllClearedCells),
            (anyDirection(D), translate(D, X, Y, CellX, CellY), astarBlocked(D, X, Y, ParentAttachments, ParentClearedCells), 
            (obstacle(CellX, CellY) ; thing(CellX, CellY, block, _)),
            not(member((CellX, CellY), ParentClearedCells)), ClearedCells = [(CellX, CellY)],
            not(((attached(CellX, CellY) ; thing(CellX, CellY, entity, Team)) )),
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
	ClearedCells = [(CellX, CellY)].

% Used to check if an agent has enough energy to perform a clear action
% checkEnergy(+CurrentEnergy, -UpdatedEnergy)       
checkEnergy(CurrentEnergy, UpdatedEnergy) :-
    clearEnergyCost(ClearCost), 
    CurrentEnergy >= ClearCost,
    UpdatedEnergy is CurrentEnergy - ClearCost.

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%% PLANNING - TASKMASTER %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Convert goalCells to distinct goalZones.
availableGoalZones(AvailableGoalZones) :-
	findall(goalCell(X, Y), goalCell(X, Y), GoalCells),
	goalCellsToGoalZones(GoalCells, [], [], GoalZones),
	findall(GoalZone, 
		(member(GoalZone, GoalZones), not(occupied(_, GoalZone))), 
		AvailableGoalZones).

goalCellsToGoalZones([], _, GoalZones, GoalZones).
goalCellsToGoalZones([goalCell(X1, Y1)|Rest], CheckedCells, FoundZones, GoalZones) :-
	member(goalCell(X2, Y2), CheckedCells), 
	distanceBetweenPoints_Euclidian(X1, Y1, X2, Y2, Dist),
	Dist =< 5,
	!,
	goalCellsToGoalZones(Rest, [goalCell(X1, Y1)|CheckedCells], FoundZones, GoalZones).
	
goalCellsToGoalZones([goalCell(X1, Y1)|Rest], CheckedCells, FoundZones, GoalZones) :-
	goalCellsToGoalZones(Rest, [goalCell(X1, Y1)|CheckedCells], [goalCell(X1, Y1)|FoundZones], GoalZones).

	
	
% Heuristic ranking of tasks - do not consider tasks more than every tenth turn.
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
	
	
% A task is complex if it is not simple
complexTask(Task) :- 
	not(simpleTask(Task)).
	
% A task is simple if:
%	- It requires no more than two blocks, all directly attached to the agent.
simpleTask(task(_, _, _, Requirements)) :-
	Requirements = [SingleReq].
	
simpleTask(task(_, _, _, Requirements)) :-
	Requirements = [req(X1, Y1, _), req(X2, Y2, _)],
	(X1 + Y1) =< 1,
	(X2 + Y2) =< 1.	


% Naively return all replies. Fails if no replies. The replies have the form:
% 	Reply = [[[GoalCell1, Resources1], [GoalCell2, Resources2], ...] , Agent]
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
		
		
% Return all the resource answers received for specific task, ordered by GoalCell
% [
%  (GoalCell1, [(agent1, [(b0, ...), (b1, ...), ...]),
%	    (agent2, [(b0, ...), (b1, ...), ...])]
%  ),
%  (GoalCell2, [...]), 
%  ...
% ]
getResourceReplies(Task, GoalCells, Replies) :-
	allResourceReplies(AllReplies),	
	findall((GoalCell, RepliesByGoalCell), 	
		(
			member(GoalCell, GoalCells),
			extractRepliesForGoalCell(GoalCell, AllReplies, RepliesByGoalCell)
		),
		Replies).


% Extract replies for the GoalCell from AllReplies.
% Return structure:
% 	(GoalCell, [(agent1, [(b0, Dist, Stops, Qty), (b1, ...), ...]), (agent2, [...]), ...]
extractRepliesForGoalCell(GoalCell, AllReplies, RepliesByGoalCell) :-
	findall((Agent, Resources),
		(
			member(Reply, AllReplies),
			Reply = [ResourcesByGoalCell , Agent],
			member([GoalCell, Resources], ResourcesByGoalCell)			
		),
		RepliesByGoalCell).
			
			
% Create tasksplan!
findPlan(Task, Answers, GoalZone, TaskPlan) :-
	% Find the best possible assignment for the given task
	bestAssignment(Task, Answers, Assignment, GoalZone, ETA),
	
	% Build the plan
	buildTaskPlanFromAssignments(Task, Assignment, ETA, TaskPlan).
		


% Recursively find best assignment - check each goalcell
bestAssignment(Task, AllAnswers, BestAssignment, BestGoalZone, BestETA) :-
	Task = task(_, _, _, Requirements),
    
	% Extract the blocks from the requirements - make each agent deliver 1 blocktype
	requirementsByBlockType(Requirements, ReqByBlockType),
	
	% Find assignment to each goal - give each goalcell a score
	findall(Assignment, 
		( 
		   member((GoalZone, AnswersForGoalZone), AllAnswers),
		   bestAssigntmentForGoalZone(Task, GoalZone, AnswersForGoalZone, ReqByBlockType, Assignment)
		),
		PossibleAssignments),
	
	% Sort the assignments and make sure we stay within the deadline
	keysort(PossibleAssignments, [BestETA-(BestGoalZone, BestAssignment)|_]).


% Find the best assignment for specific goalZone.
bestAssigntmentForGoalZone(Task, GoalZone, Answers, Requirements, Assignment) :-
	Task = task(TaskName, Deadline, _, _),
	
	% First, ensure we have more agents than blocktypes
	length(Answers, NrOfAnswers),
	length(Requirements, NrOfRequirements),
	NrOfAnswers >= NrOfRequirements,
	
	% Find all possible Agent Assignments
	findall(MaxDistance-Match,
			( 
				matchAgentsToRequirements(TaskName, GoalZone, Answers, Requirements, [], Match),
				findall(Distance, member((_, _, Distance, _, _, _, _), Match), Distances),
				max_list(Distances, MaxDistance)
			),
			Matches),
	
	% Find the assignment with the best score
	min_member(BestMatchDistance-BestMatch, Matches),
	
	% Attach the score of the match
	step(Step),
	ETA is Step + (BestMatchDistance),
	ETA =< Deadline,
	Assignment = ETA-(GoalZone, BestMatch).
		
		
		

%%% matchAgentsToReuirements(TaskName, AgentAnswers, Requirements, SubmitAgentPlan, OtherAgentPlans, Match)

% If we have no answers left, but there are still requirements to be satisfied, return the empty match.
matchAgentsToRequirements(_, _, [], [_|_], _, []).

% If there are no requirements left, we have succesfully found a matching.
matchAgentsToRequirements(_, _,  _, [], PartialMatch, Match) :-
	PartialMatch \= [],
    	Match = PartialMatch.
	
% Match an agent with a requirement 
matchAgentsToRequirements(TaskName, GoalZone, Agents, 
                          [(BlockType, ReqQty, Positions)|OtherRequirements],
                          PartialMatch, Match) :-	
			  
	% Find an agent from answers and check what block it can deliver
	select((Agent, BlockList), Agents, AgentsRest),
	not(member((Agent, _, _, _, _, _, _), PartialMatch)),
	member(Block, BlockList), 
	Block = (BlockType, Dist, Stops, BlockQty),
	
	% Ensure the agent can deliver the necessary quantity
	ReqQty =< BlockQty,
	
	% Append the goalZone to the agentplan
	agentOffset(Agent, X, Y),
	OffsetX is -X, OffsetY is -Y,
	addOffset(OffsetX, OffsetY, GoalZone, OffsetGoalZone),
	append(Stops, [OffsetGoalZone], Path),
	AgentPlan = (Agent, TaskName, Dist, BlockType, ReqQty, Path, Positions),
	
	% Add the agent to the plan
	matchAgentsToRequirements(TaskName, GoalZone, AgentsRest, 
				  OtherRequirements, 
				  [AgentPlan |PartialMatch], Match).



% Collect requirements into sets by distance and blocktype.
%%% OBS!! - Has been modified to accomodate only single block attachments
requirementsByBlockType(Requirements, FilteredRequirements) :-
	findall((BlockType, 1, [(X, Y)]),
		member(req(X, Y, BlockType), Requirements),
		FilteredRequirements).
	
	
buildTaskPlanFromAssignments(Task, Assignments, ETA, TaskPlan) :-
	% Find the task and requirements
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%% PLANNING - OTHER AGENTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Return the available resources for specific task and each specified goalcell.
% The Resources list has the following format:
%	[
%		[ goalCell1 , 
%			[ (BlockTypeA, Dist, Stops=[Stop1, Stop2, ...] , Qty] ),
%			  ...
%			]
%		],
%		[ goalCell2, 
%			...
%		],
%	]
%
% OBS! For now any agent is only allowed to make a single stop on its' path to the goalZone.
getResourcesForTask(Task, GoalCells, Resources) :-
	% For each given goalCell, find fastest ways to get each block type
	findall(ClosestResources,
		( 
			member(GoalCell, GoalCells), 
		  	closestResourcesToGoalCellForTask(GoalCell, Task, ClosestResources) 
		),
		Resources).
		

closestResourcesToGoalCellForTask(GoalCell, Task, ClosestResourcesToGoalCell) :-
	% Find offset for the taskMaster
	taskMaster(TaskMaster), 
	agentOffset(TaskMaster, OffsetX, OffsetY),
	addOffset(OffsetX, OffsetY, GoalCell, OffsetGoalCell),
	
	% Find requirements and deadline
	task(_, Deadline, _, Requirements) = Task,
	step(Step),
	MaxDist is (Deadline - Step),
	
	% Find all known block types
	findall(BlockType, member(req(_, _, BlockType), Requirements), RepeatingBlockTypes),
	sort(RepeatingBlockTypes, BlockTypes),

	% Find closest resource of each blocktype
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
	% Check if we have any blocks of that type attached
	findall(_, attachedToMe(_, _, block, BlockType), AttachedBlocks), length(AttachedBlocks, NrOfAttachedBlocks),
	
	% Find closest resource
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
	



%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXECUTING THE PLAN %%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
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



rotationRequiredTask(BlockType, X, Y, R) :-
	not(attachedToMe(X, Y, block, BlockType)),
	attachedToMe(AttachedX, AttachedY, block, BlockType),
	rotation(R, AttachedX, AttachedY, X, Y, Angle),
	(Angle = 90 ->
			not(blockedRotation(R, 90));
			(not(blockedRotation(R, 90)), not(blockedRotation(R, 180)))
	).
	
clearRequiredTask(BlockType, X, Y, Xc, Yc) :-
	not(attachedToMe(X, Y, block, BlockType)),
	attachedToMe(AttachedX, AttachedY, block, BlockType),
	rotation(R, AttachedX, AttachedY, X, Y, Angle),
	(Angle = 90 ->
			blockedRotation(R, 90) ;
			(blockedRotation(R, 90) ; blockedRotation(R, 180))
	),
	translate(_, 0, 0, Xc, Yc), thing(Xc, Yc, obstacle, _).



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
	
	
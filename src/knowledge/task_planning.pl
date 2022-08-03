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
	ClosestResource = (BlockType, [DistToGoal], [], Qty).


closestResourcesWithoutAttachedBlocks(BlockType, GoalCell, MaxDist, ClosestResource) :-
	Qty is 2,
        closestDispenserToGoal(GoalCell, BlockType, Dispenser, DistWithDispenser),
        DistWithDispenser < MaxDist,
        ClosestResource = (BlockType, [DistWithDispenser, Dispenser], [], Qty).
        
        	
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
	
	
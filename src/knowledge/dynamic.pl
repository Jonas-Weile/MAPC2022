 :- dynamic  
 	%%%%%% COMMON %%%%%%
	attachedToMe/2,		% Things attached to the agent 
	enumDirList/2, 		% Pseudo-random list of enumerated directions
	clearEnergyCost/1, clearSteps/1, clearStepsCounter/1, 
	completedClearAction/1,	% Clearing related beliefs
	maxEnergy/1, 		% Maximum energy
	myRole/1,		% Current role
	vision/1,		% Vision radius
	
	% Initial percepts
	name/1, team/1, teamSize/1, steps/1, role/6,
	
	% Request-Action percepts
	accepted/1, attached/2, deactivated/1, energy/1, goalZone/2, hit/2, 
	lastAction/1, lastActionResult/1, lastActionParams/1,
	norm/5, roleZone/2, surveyed/4, surveyed/2, score/1, 
	step/1, task/4, thing/4, violation/1,
	
	% Permanent things
	dispenser/3, goalCell/2, roleCell/2,
	
	% myPosition(?X, ?Y) - Current position of agent
	myPosition/2,
	
	% visited(?X, ?Y, ?Step)
	visited/3,
	
	
	%%%%%% COMMUNICATION %%%%%%
	commonEnvironmentPercepts/3,
	savedCommonEnvironmentPercepts/3,
	
	% agentOffset(Agent, OffsetX, OffsetY)
	agentOffset/3,
	
	% Simply used to temporarily store new connections
	newConnection/3,
	
	% Connections
	connectionRequest/5,
	connectionUpdate/1,
	connectionToInfo/2, 
	newConnections/3,
		
		
	%%%%%% GOALS %%%%%%
	actionPlan/1, explore/0, fullyEquipped/0, fullyConnected/0, findMapDimensions/0, goTo/2, restart/0,
	
	
	%%%%%% PLANNING %%%%%%
	taskMaster/1, submitAgent/1, submitted/1,
	taskPlan/1, taskPlan/7, taskPlanToDo/1,
	
	% Messages
	taskSubmitted/1, dropTask/1, deleteTask/1,
	
	% Used to mark information to share if I'm part of a team
	share/1,
		
	% resource requests
	resourceRequest/2, resourceRequestSent/2, resourceReply/1, savedResourceReply/1,
	
	% connectionFromTo
	connectionFromTo/3, connectionFromTo/5,
	
	% occupied thing by some agent
	occupied/2, taskTaken/2, checkedTask/1, blockDelivered/1,
	waypointsToGoal/2,
	
	%%%%%%%%%% THESE ARE SIMPLY TO STOP GOAL FROM COMPLAINING %%%%%%%%%
	translateToMyOrigin_Agent/2, findNC_folder/1, collectListsToSets/0, gcd_helper/0,
	connectedBlocks_folder/0, rankTask/0, buildAgentPlanFromAssignments/1, relativeToAbsolutePositionOfPoints/0.
include(knowledge_test1.pl).
:- dynamic

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
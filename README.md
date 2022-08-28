# GOAL-DTU

This is the reporsitory of the GOAL-DTU team for the 2022 Multi-Agent Programming Competition. The code is mostly reused from the previous iteration, with only a small number of changes made to adapt the code to the new scenario, but no changes to actually update the logic. The most recent branch is the ***qualification*** branch.


---
### Module Organization
---
[communication](./src/modules/communication/) - contains all logic for communication between agents; this includes connecting with other agents and sharing knowledge.
   

[goals](./src/modules/goals/) - contains all logic involved with setting/updating goals.
   
   
[main](./src/modules/main/) - contains the *'main'* modules - these are the modules directly called from [mapcMain.mod2g](/src/mapcMain.mod2g).
   
   
[percepts](./modules/percepts/) - contains all modules for updating and initializing the percepts of the agent.
   

[roles](./src//modules/roles/) - currently not used.
   

[taskplanning](./src/modules/taskplanning/) - contains all logic needed for **creating** taskplans.


---
### Current Strategy
---

#### Tasks and Task Planning
The agents elect a single agent as *TaskMaster*. The taskmaster will prompt agents for available resources and create *TaskPlans* to solve available tasks.
A taskplan is highly specified with a static assembly point, an elected *submit-agent*, as well as a number of agents to supply the necessary blocks. 
The taskmaster chooses the assembly point among the known goalcells, and all agents then calculate where to deliver their blocks respective to this static point.

***OBS.*** this means, that at this point in time, a taskplan will completely fail if the assembly point is blocked by another agent or similar, as the assembly point is not dynamically recomputed - see the section [TODO](https://github.com/Jonas-Weile/MAPC2022/edit/main/README.md#todo).

#### Roles
The current iteration of the code does not take advantage of the respective strengths/weaknesses of the different roles. Instead, all agents will change their role from *'default'* to *'worker'* at the first given opportunity, and then stick with this role throughout the simulation.

#### Norms
Norms are completely ignored. The *norm percepts* are saved by each agent, but the agent contain no logic to deal with norms.

   
---
### Knowledge
---
The built-in *knowledge* of the agents is split into several different *'prolog knowledge files'* mostly following the same scheme as for **modules**. However, for GOAL to work properly (or at least the easiest way to make it so) is to collect all knowledge into a single file. Therefore, we have created a [batch file](./resources/myscript.bat) that collects all knowledge into a single file.

The batch file can be executed directly from eclipse:
   1. Go to the navigation bar and open *run*.
   2. Click *External Tools*.
   3. Click *External Tools Configuration*.
   4. Add a new launch configuration.
      - Under location, specify the path to the batch file.
      - As *Working Directory*, use the *resources* folder.
   5. Click *'Apply'*.
   6. Click *'Run'*.

The batch file should now have been run. Further, now that is added, it should be possible to run the script directly from the toolbar (under the navigation bar) - see the button next to the *debug* and normal *run* buttons. 



---
### TODO
---

- **GoalZones** - goal zones have some small probability of moving! This is not accounted for; at the moment we consider goalZones as *static* knowledge.

- **Delivering Tasks** -
   1. Make agents better at solving problems by clearing - eg. if it cannot connect a block etc.
   2. Whenever an agent performs a random move (etc. when it cannot attach a block), do something better - the strategy with random moves results in a small probability of getting stuck.
   3. Make the agent able to detach blocks when necessary.
   
   4. Make pattern building more flexible - ex. by allowing agents to build pattern outside goal zone, then submit inside, or simply by letting the agents decide on a meeting point themselves.


- **A-Star** - The *A-star search with clear* does not seem to work properly. Some changes to the original logic have already been made, but no true effort has been spent trying to make it work.

- **Offsets and Map Dimensions** - there seems to be a small error in either the computation of offsets, or the computation of map dimensions. This can sometimes result in the agents believing in a wrong set of dimensions, which potentially unleashed total chaos - ***THEREFORE THE MAP DIMENSIONS ARE NOT INFERRED AT THE MOMENT***, i.e. the agents treat the map as infinite, not as a torus.

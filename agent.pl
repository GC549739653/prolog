% COMP9414 Assignment3 (Semester 1, 2018)
%
% Guo Chang (z5140582)
% Yang Fengting(z5089358)
% hw3group Number: 362
% 
% Project 3, Option 2: Prolog (BDI Agent)
%
% In this Assignment, you will be implementing an agent to move around in a rectangular environment, picking up stones from the land and dropping them in the water.
% Thus building a path to the location of a dangerous sea monster. The final stone must be dropped on the head of the sea monster in order to kill it.

%############################################################################################################################################################################################
%The initial conditions are set to avoid throwing an exception during program execution.
:- dynamic judge_ground/3.
%Set to dynamic in order to prevent the occurrence of false....
:- discontiguous solve/4.
:- discontiguous sub_D/3.
:- discontiguous sub_P/3.
%This part is used to suppress some warning messages.....

%----------------------------------------------Q1----------------------------------------------------
%which binds Intentions to intents(L,[]) with L in the form [[goal(X1,Y1),[]], ... , [goal(Xn,Yn),[]]]. 
%Here (Xn,Yn) is the location of the monster and (X1,Y1), ... , (Xn-1,Yn-1) are places where the mininum number of stones need to be dropped,
% in order to allow the agent to move along some path from its current location to that of the monster.

%This is a function used to delete the elements in the list. A is to delete a single element. 
%B is a general form of a type of element, that is, to delete all elements in the B form.
delete_process(A,B):-
	%writeln('ssssssssssssssssss')
	retract(A),
	retractall(B).

%Used to locate the agent and insert it into the list.
where_is_agent(XAGENT,YAAGENT):-
	agent_at(XAGENT,YAAGENT),
	assert(goal(goal(XAGENT,YAAGENT))).
	
%Delete single element.	
delete_agent_pos(A,B):-
	retract(goal(goal(A,B))).

%A simple cut,means the previous selection does not need to be reconsidered when backtracking the chain of satisfied goals.	
judge():-
	!.


%Used to calculate the distance.
abs(A, B, D) :-
    D is A - B, D >= 0, !.

abs(A, B, D) :-
    D is B - A.

%This is a range function, K is a number between M and N.	
range(N, M, K):- 
	N < M, K = N.
range(N, M, K):- 
	N == M, 
	!,
	K = N.
range(N, M, K):- 
	N < M, 
	N1 is N+1, 
	range(N1, M, K).
%range(1,5,K).

%In this question, the ground should have three states, one is water, the other is land, and the last is water filled with stones.
%The following functions are used to determine the ground conditions.

%The ground or is filled by stones, and it can pass through. The cost is set to 1.
judge_ground(goal(A,B),goal(C,D),1):-
	land_or_dropped(C,D),%on the land
	abs(A, C, Difx),
    abs(B, D, Dify),
    1 is Difx + Dify.%The two points are separated by 1 Manhattan distance.
	
%How to describe the next position is water.The cost is set to 6666.
% A-1=<C<=A+1,B-1<=D<=B+1.
judge_ground(goal(A,B),goal(C,D),6666):-	
	AADD1 is A+1,BADD1 is B+1,
	ALESS1 is A-1,BLESS1 is B-1,
	range(BLESS1,BADD1,D),
	range(ALESS1,AADD1,C),
	not(land_or_dropped(C,D)),%NOT LAND
	abs(A, C, Difx),
    abs(B, D, Dify),
    1 is Difx + Dify.%%The two points are separated by 1 Manhattan distance.

%Here we use ucs's solve algorithm to get a path.
solve(goal(A,B),Solution,G,N):-
	%ucsdijkstra([[goal(A,B),goal(A,B),0]], [], Solution, G, 1, N),
	%gg = goal(A,B),
	N=1,G=2,
	goal(goal(A,B)),
	AADD1 is A+1,BADD1 is B+1,
	ALESS1 is A-1,BLESS1 is B-1,
	range(BLESS1,BADD1,D),%It should be noted here that the abscissa priority or ordinate priority, we get the path is different.
	range(ALESS1,AADD1,C),% we get the path is different.
	land_or_dropped(C,D),
	abs(A, C, Difx),
    abs(B, D, Dify),
    1 is Difx + Dify,
	%so1=goal(C,D),
	%so2 = goal(A,B),	
	Solution=[goal(C,D),goal(A,B)].	
	%write(Solution),
	
%Initially, the agent is at (1,1), monster at (9,9).
%
initial_intentions(intents(L,[])):-
	%write('ssssssssss'),
	where_is_agent(XAGENT,YAAGENT),%Insert the agent location.
	monster(XMONSTER,YMONSTER),
	%ag = goal(goal(XAGENT,YAAGENT)),
	%Amo =goal(XMONSTER,YMONSTER),	
	solve(goal(XMONSTER,YMONSTER),Path,_,_),%get path
	%write(Path)
	%write('Initial Goals: '),
	%%Find all eligible goals
	%update_action(Path,L),
	findall(X,(member(goal(GOAL1,GOAL2),Path),not(land(GOAL1,GOAL2)),X=[goal(GOAL1,GOAL2),[]]),L),
	%When we use an agent position, the agent is moved and the previous position should be deleted.
	%Otherwise there are two starting points and the program will go wrong.
	delete_process(goal(goal(XAGENT,YAAGENT)),judge_ground(_,_,6666)).
	
%-------------------------------------------------Q2--------------------------------------------------------------------------------------------------
%which takes a list of percepts, each of the form stone(X,Y), and converts it into a corresponding list of goals, each of the form goal(X,Y).

%We can see from the trial.txt that the stone(X,Y) in Percepts is the same as the goal(X,Y) in Goals.
%So we can do the conversion, here we can use a variety of methods, such as maplist, recursive, fold, etc.
%I choose findall since it is simple and is a builtin func..
trigger(Percepts, Goals):-
	%update_action(Percepts,Goals,goal,stone).
	findall(S,(S = goal(GOAL1,GOAL2),member(stone(GOAL1,GOAL2),Percepts)),Goals).
	%write('Percepts'),
	%write('Goals'),
	%Select the stone() in Percrpts and convert it to goal(), and save it in Goals to get the answer.
	
	
%--------------------------------------------------Q3-----------------------------------------------------------------------------------------------------
%Write a Prolog procedure:incorporate_goals(Goals, Intentions, Intentions1)
%This procedure should take two inputs, as follows:
%a set of Goals in the form of a list [goal(X1,Y1), ... , goal(Xn,Yn)]
%the current Intentions of the agent, in the form intents(Int_drop,Int_pick) where Int_drop, Int_pick are lists of intentions in the form [goal(X,Y), Plan]
%Your procedure should return the updated Intentions of the agent after inserting the new goals into Int_pick. 
%The new goals should be inserted into the existing list in decreasing order of the length of the shortest valid path from the agent's current position. 
%A valid path is one which passes through only locations (X,Y) for which land_or_dropped(X,Y) is true. More precisely, 
%a new goal should be placed immediately before the first goal in the list whose path length is longer than that of the new goal (without reordering the current list of goals). 
%If no such valid path exists, then the new goal should not be inserted. 
%Note that because of repeated perception of the same event, only goals not already in the list should be inserted into the list of intentions. 
%The Plan associated with each new goal should be the empty plan (represented as the empty list []).

%####################################################################################################################################
%The third question, I was always killed in the autotest on give, I modified it myself many times, and did not find a solution.     #
%but I was good at local testing. If you can test my code locally, then I will be very grateful！！！！                             #
%####################################################################################################################################
%Call insert intent function.
use_add_function(GoalsHead,Intentions,Intentionswhere):-
	add_next_intent(GoalsHead,Intentions,Intentionswhere).
	
%Update the intentions through the goal.
%We have two cases, one is picked and the other is dropped. Here, dropped doesn't need to be modified. It only needs to deal with the case of pick.
%According to the length of the path to pick postion if there is a vaild path, sort in ascending order.

incorporate_goals([], Intentions, Intentions):-
	judge().

%First, insert the initial point.
%Second,insert what we want .
%Third, delete the initial point.
%Last，recursive.
incorporate_goals([GoalsHead|GoalsTail], Intentions, Intentions1):-
	%insert goal point.
	where_is_agent(XAGENT,YAAGENT),
	%call add funtion to insert.
	use_add_function(GoalsHead,Intentions,Intentionswhere),
	%Delete the used point.
	delete_agent_pos(XAGENT,YAAGENT),
	incorporate_goals(GoalsTail,Intentionswhere,Intentions1),%recursive.
	judge().%cut!
	
%For convenience, the solve function is unified.
judge_solve(A,B,C,D):-
	solve(A,B,C,D).

%If there is no path, cut.	
use_add_sub_not(A):-
	not(judge_solve(A,_,_,_)),
	judge().
	
%You should find the path before inserting.
%A is goal ,B is path length,C is pick ,D is another pick.
use_add_sub_sol(A,C,D):-
	judge_solve(A,_,B,_),
	sub_add(A,B,C,D),
	judge().

	
%Since inserting the goal may change the order, we should insert the goals one by one. Here we define a predicate that inserts a single goal as an aid.
add_next_intent(Goal,intents(Drop,Pick),intents(Drop,Pick)):-%no path, not(judge_solve(A,_,_,_)),
	use_add_sub_not(Goal).
add_next_intent(Goal,intents(Drop,Pick),intents(Drop,Pickanother)):-%Call a find path function.
	use_add_sub_sol(Goal,Pick,Pickanother).	

%a simple compare function.
compare(A,B,C,D,E):-
	A>=B,
	sub_add(C,A,D,E).
compare(A,B,_,_,_):-
	A<B,
	judge().

%call sub_add and means we begin recursive.
recall_sub_add(A,B,C,D):-	
	sub_add(A,B,C,D).
	
	
sub_add(Goal,_,[],[[Goal,[]]]).

%If the same situation occurs, then we do not continue to operate.
sub_add(Goal,_,[[Goal,Plan]|Tail],[[Goal,Plan]|Tail]).%cut

%The headplan may be empty and not planned.
%So we can't use its length as a path length for comparison, so we solve it and get LEN.
sub_add(Goal,G,[[HeadGoal,Headplan]|Tail],[[HeadGoal,Headplan]|Tailanother]):-
	judge_solve(HeadGoal,_,LEN,_),
	%compare(G,LEN,Goal,Tail,Tailanother).
	%write('ssssssssss'),
	%write(LEN),
	%write('ssssssssss'),
	%write(G),
	X is G,
	Y is LEN,
	X>=Y,
	%it means we should go on recursive ...
	recall_sub_add(Goal,G,Tail,Tailanother).%recursive

sub_add(Goal,G,[[HeadGoal,Headplan]|Tail],[[Goal,[]],[HeadGoal,Headplan]|Tail]):-
	judge_solve(HeadGoal,_,LEN,_),
	%compare(G,LEN,_,_,_).
	X is G,
	Y is LEN,
	Y>X,
	%it means we should stop.....then judge and stop.
	judge().

	
%------------------------------------------------------------------------Q4----------------------------------------------------------------------
%Write a Prolog procedure:get_action(Intentions, Intentions1, Action)
%which takes the agent's current Intentions in the form intents(Int_drop,Int_pick) (as described above) and computes an action to be taken by the agent as well as the updated Intentions. 
%The agent should select an intention as follows:
%If the agent is currently holding a stone, indicated by agent_stones(1), then the first intention [goal(X,Y), Plan] in the list Int_drop of dropping intentions is selected;
%otherwise, if the list Int_pick of picking intentions is not empty, then its first item [goal(X,Y), Plan] is selected;
%otherwise, no intention is selected; in this case, the agent's Intentions should remain as they are, and it should stay in its current location (i.e. action is move(X,Y) if it is currently at (X,Y)).

%We consider the goals and plans of the first element in intentpick and intentdrop.
%When agent_stones(1), we consider intentdrop.
%When agent_stones(0), we consider intentpick.
%otherwise ,we stay .


	
get_action(intents(Drop,Pick), intents(Dropanother,Pick), Action):-
	agent_stones(1),%agent has stones. drop.
	where_is_agent(XAGENT,YAAGENT),%insert agent position.
	sub_D(Drop,Dropanother,Action),%(+,-,-),call drop action.
	delete_agent_pos(XAGENT,YAAGENT),%delete used position.
	judge().%cut
	
get_action(intents(Drop,Pick), intents(Drop,Pickanother), Action):-
	agent_stones(0),%agent without stones.pick.
	where_is_agent(XAGENT,YAAGENT),%insert agent position.
	sub_P(Pick,Pickanother,Action),%(+,-,-),call pick action.
	delete_agent_pos(XAGENT,YAAGENT),%delete used position.
	judge().%cut.
	
sub_D([],[],move(XAGENT,YAAGENT)):-
	agent_at(XAGENT,YAAGENT).%get agent position.
	
sub_D([[goal(X,Y),[Headplan|Tailplan]]|Tailpick],[[goal(X,Y),Tailplan]|Tailpick],Headplan):-
	%judge Headplan is applicable. Action = Headplan,Plan = Tailplan
	applicable(Headplan).
	
%sub function to append what we want an call findall to transfer action to move,and get a new plan.
append_and_find(A,X,Y,B,C):-	
	append([_|Tail],[_],A),
	update_action(Tail,Tailp),%update a new plan.
	append(Tailp,[drop(X,Y)],[B|C]).%update a drop action.
	
%sub function to append what we want an call findall to transfer action to move, and get a new plan.
append_and_find_(A,X,Y,B,C):-
	append([_|Tail],[_],A),
	update_action(Tail,Tailp),%update a new plan.
	append(Tailp,[pick(X,Y)],[B|C]).%update a pick action.
	
sub_D([[goal(X,Y),[]]|Tailpick],[[goal(X,Y),Tailnewplan]|Tailpick],Action):-
	%aa = goal(X,Y),
	judge_solve(goal(X,Y),Path,_,_),%get a plan path.
	append_and_find(Path,X,Y,Action,Tailnewplan).%update plan.

	
sub_D([[goal(X,Y),[Headplan|_]]|Tailpick],[[goal(X,Y),Tailnewplan]|Tailpick],Action):-
	%if not applicable,goal(X,Y)  as  solve(Start,_,_,_)
	not(applicable(Headplan)),
	%aa = goal(X,Y),
	judge_solve(goal(X,Y),Path,_,_),%path as newplan.
	append_and_find(Path,X,Y,Action,Tailnewplan).%update plan.

%Prevents some exceptions
sub_D(goal(X,Y),_):-
	judge_solve(goal(X,Y),Path,_,_),
	append([_|_],[_],Path).
sub_D([[goal(X,Y),[_|_]]|_],goal(X,Y),Action):-
	judge_solve(goal(X,Y),Path,_,_),
	append([_|_],[_],Path),
	append(_,[drop(X,Y)],[Action|_]).

	
sub_P([],[],move(XAGENT,YAAGENT)):-
	agent_at(XAGENT,YAAGENT).	%get agent position.
	
sub_P([[goal(X,Y),[Headplan|Tailplan]]|Tailpick],[[goal(X,Y),Tailplan]|Tailpick],Headplan):-
	applicable(Headplan).%judge Headplan is applicable. Action = Headplan,Plan = Tailplan
	
sub_P([[goal(X,Y),[]]|Tailpick],[[goal(X,Y),Tailnewplan]|Tailpick],Action):-
	%aa = goal(X,Y),
	judge_solve(goal(X,Y),Path,_,_),%get a plan
	append_and_find_(Path,X,Y,Action,Tailnewplan).%update our plan.
	
	
sub_P([[goal(X,Y),[Headplan|_]]|Tailpick],[[goal(X,Y),Tailnewplan]|Tailpick],Action):-
	not(applicable(Headplan)),%if not applicable,goal(X,Y)  as  solve(Start,_,_,_)
	%aa = goal(X,Y),
	judge_solve(goal(X,Y),Path,_,_),%get new plan.
	append_and_find_(Path,X,Y,Action,Tailnewplan).%update our plan.

%Prevents some exceptions
sub_P(goal(X,Y),_):-
	judge_solve(goal(X,Y),Path,_,_),
	append([_|_],[_],Path).
sub_P([[goal(X,Y),[_|_]]|_],goal(X,Y),Action):-
	judge_solve(goal(X,Y),Path,_,_),
	append([_|_],[_],Path),
	append(_,[pick(X,Y)],[Action|_]).

%Used to find or modify the elements that satisfy the condition and store it in a new list.
update_action(Plan,Newplan):-
	findall(SS,(SS=move(GOAL1,GOAL2),member(goal(GOAL1,GOAL2),Plan)),Newplan).

%-----------------------------------------------------Q5---------------------------------------------------------------------------	
%Write a Prolog procedure:update_intentions(Observation, Intentions, Intentions1)
%to update the agent's intentions, based on observation. An at(X,Y) observation should not change the agent's intentions. 
%In the case of a picked() or dropped() observation, 
%the agent should remove the corresponding plan from its list of intentions (since this plan has now successfully been executed).

%observation is at means do nothing.
update_intentions(at(_,_), Intentions, Intentions).
%if is drop,then we delete intentdrop's first element.
update_intentions(dropped(_,_),intents([_|TailDrop],Pick),intents(TailDrop,Pick)).
%if is pick,then we delete intentpick's first element.
update_intentions(picked(_,_),intents(Drop,[_|TailPick]),intents(Drop,TailPick)).


%------------------------------------------------------------------------------------------------------------------------
%Below is the pathsearch and ucs code from the teacher.
%------------------------------------------------------------------------------------------------------------------
% pathsearch.pl

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% This file provides code for insert_legs(), head_member() and build_path()
% used by bfsdijkstra(), ucsdijkstra(), greedy() and astar().

% insert_legs(Generated, Legs, Generated1).
% insert new legs into list of generated legs,
% by repeatedly calling insert_one_leg()

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).


%------------------------------------------------------------------------------------
% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.


solve(Start, Solution, G, N) :-
    %consult(pathsearch), % insert_legs(), head_member(), build_path()
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N).

% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    goal(Node),
    build_path([[Node,Pred]|Expanded], Path).

% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (judge_ground(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).

% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).

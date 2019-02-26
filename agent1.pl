mandist(X/Y, X1/Y1, D) :-      % D is Manhattan Dist between two positions
    dif(X, X1, Dx),
    dif(Y, Y1, Dy),
    D is Dx + Dy.

dif(A, B, D) :-                % D is |A-B|
    D is A-B, D >= 0, !.

dif(A, B, D) :-                % D is |A-B|
    D is B-A.
%------------------------------------------------------------------------------------
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
% Breadth First Search, using Dijkstras Algorithm

% COMP3411/9414 Artificial Intelligence, UNSW, Alan Blair

% This is a memory-efficient implementation of BFS, which is a
% special case of Dijkstras algorithm. It is designed to find
% only one solution (which is guaranteed to be the shortest path).

% solve(Start, Solution, D, N)
% Solution is a path (in reverse order) from start node to a goal state.
% D is the depth of the path, N is the number of nodes expanded.

solve(Start, Solution, D, N)  :-
    consult(pathsearch), % head_member(), build_path()
    bfsdijkstra([[Start,Start]], [], Solution, 1, N),
    length(Solution, D1),
    D is D1 - 1.

% bfsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]
% sorted in increasing order of path length from the start node.
% Expanded nodes are moved to a separate list.

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
bfsdijkstra([[Node,Pred]|_Generated], Expanded, Path, N, N)  :-
    goal(Node),
    build_path([[Node,Pred]|Expanded], Path).

% Take the the leg at the head of the queue and extend it,
% by generating the successors of its destination node.
% Append these new legs to the back of the queue, and keep searching.
bfsdijkstra([[Node,Pred]|Generated], Expanded, Solution, L, N) :-
    extend(Node, Generated, Expanded, NewLegs),
    M is L + 1,
    append(Generated, NewLegs, Generated1),
    bfsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been generated or expanded.
extend(Node, Generated, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode,Node], (s(Node, NewNode, _)
    , not(head_member(NewNode, Generated))
    , not(head_member(NewNode, Expanded))
    ), NewLegs).

%------------------------------------------------------------------------------------------------
s(state(A1,B1),state(A2,B2),1):-
	land(A2,B2),
	mandist(A1/B1,A2/B2,1).
	
s(state(A1,B1),state(A2,B2),1000):-	
	between(1,1000,A2),
	between(1,1000,B2),
	not(land(A2,B2)),
	mandist(A1/B1,A2/B2,1).
goal(state(1,1)).



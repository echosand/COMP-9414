%this part should be the 1st part of the assignment, which is modified with the UCSdijkstra.pl and pathsearch.pl, we have change the format of solve by add a parameter of the sequence of question using these searching part.

% Firstly we define the goal of every path to be the current location of
% the agent.
goal((X,Y)):-
    agent_at(X,Y).

%solve(Start, Solution, G, N,Q),Q is the new parameter to match with the proper question.
solve(Start, Solution, G, N,Q)  :-
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N,Q).
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N,_)  :-
    goal(Node),
    build_path([[Node,Pred]|Expanded], Path).
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N,Q) :-
    extend(Node, G, Expanded, NewLegs,Q),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N,Q).
extend(Node, G, Expanded, NewLegs,Q) :-
    Q = 3,
    findall([NewNode, Node, G1], (s3(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).
extend(Node, G, Expanded, NewLegs,Q) :-
    Q = 1,
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).
insert_one_leg([], Leg, [Leg]).
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).
insert_legs(Generated, [], Generated).
insert_legs(Generated, [Leg|Legs], Generated2) :-
    insert_one_leg(Generated, Leg, Generated1),
    insert_legs(Generated1, Legs, Generated2).
head_member(Node,[[Node,_]|_]).
head_member(Node,[_|Tail]) :-
    head_member(Node,Tail).
build_path([[Next,Start],[Start,Start]], [Next,Start]).
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
    build_path([[B,A]|Expanded],[B,A|Path]), ! .
build_path([Leg,_SkipLeg|Expanded],Path) :-
    build_path([Leg|Expanded],Path).
%Q1
%we define every extend node by function p() and every node should be in the scope of (0,10),(0,10).
p((X,Y),(X1,Y)):-
	X1 is X-1,
	X1>=0,
	X1<10.
p((X,Y),(X1,Y)):-
	X1 is X+1,
	X1>=0,
	X1<10.
p((X,Y),(X,Y1)):-
	Y1 is Y+1,
	Y1>=0,
	Y1<10.
p((X,Y),(X,Y1)):-
	Y1 is Y-1,
	Y1>=0,
	Y1<10.

%then we define the function s() to find the least path.
%when the start and goal are all on the land, it cost no stones, so cost should be 0.
s((X,Y), (X1,Y1), 0) :-
    p((X,Y), (X1,Y1)),
    land(X,Y),land(X1,Y1).

%Otherwise when the start and goal are not all on the land, every step need one stone, so cost should be 1.
s((X,Y), (X1,Y1), 1):-
    p((X,Y), (X1,Y1)).

%Because the initial_intentions should show only the water path (without the land path), so we define function showsol()to select the water points(not(land(X,Y))).
showsol([],[]).
showsol([(X,Y)|L],[(X,Y)|NL]):-
    not(land(X,Y)),
    showsol(L,NL).

showsol([(X,Y)|L],NL):-
    land(X,Y),
    showsol(L,NL).

%set the start point with the monsters location, and use ucs algorithm to find the shortest path, and finally show the water points where need stones to drop.
intention1(Intention):-
	monster(X0,Y0),
	solve((X0,Y0),Y,_,_,1),showsol(Y,Intention).

%change the format of the result of former program:
%(X,Y)->(intents([[goal(X,Y),[]]...],[]))
change(intents(X,[]),X).
intension2([[goal(X,Y),[]]|T1],[(X,Y)|T]):-
    intension2(T1,T).
intension2([],[]).

% combine all the previous processes by the output function: initial_intentions()
initial_intentions(Intentions):-
	intention1(R1),intension2(R2,R1),change(Intentions,R2).

%Q2
% base case to stop the recursion.
trigger([],[]) :-!.

%change the format from [stone(X,Y)...] to [goal(X,Y)...]
trigger([stone(X,Y)|Percepts], [goal(X,Y)|Goals]) :-
	trigger(Percepts, Goals).

%Q3
% Same as Q1, we define every extending nodes by function:p3(),every
% nodes should be land_or_dropped(X,Y).
p3((X,Y),(X1,Y)):-
	X1 is X-1,
	X1>=0,
	X1<10,
    land_or_dropped(X1,Y).
p3((X,Y),(X1,Y)):-
	X1 is X+1,
	X1>=0,
	X1<10,
    land_or_dropped(X1,Y).
p3((X,Y),(X,Y1)):-
	Y1 is Y+1,
	Y1>=0,
	Y1<10,
    land_or_dropped(X,Y1).
p3((X,Y),(X,Y1)):-
	Y1 is Y-1,
	Y1>=0,
	Y1<10,
    land_or_dropped(X,Y1).

% s3() is defined to find the shortest path of every stone using ucs and
% pathsearch.
s3((X,Y), (X1,Y1), 1):-
    p3((X,Y), (X1,Y1)).

% This function used for transfering the format of
% goals([goal(X,Y),goal(X1,Y1)]) into new
% goals([[goal(X,Y),[]],[goal(X1,Y1),[]]]).
trigger2([goal(X,Y)|T],[[goal(X,Y),[]]|T1]):-
    trigger2(T,T1).
trigger2([],[]).

%this function used to judge if the goal has existed in the queue.
member([H|T],X):-
    H \= X,
    member(T,X).
member([H|_],X):-
    H = X.

% This represents a special case: when the stone just appear at the same
% place with the current location of agent, the distance should be
% regarded as 2 not 0.
distance1((X,Y),(X,Y),2).
distance1((X,Y), (X1,Y1), D) :-
    dif(X, X1, Dx),
    dif(Y, Y1, Dy),
    D is Dx + Dy.
% We need to compare the distance between all the effective goals and
% agent_at location, and sort them by the dicreasing order, so we try to
% one by one insert the new goals into the queue by defining function
% l().

l(N,[],[N]).
l([goal(X1,Y1),A],[[goal(X,Y),B]|[]],[[goal(X1,Y1),A],[goal(X,Y),B]]):-
    agent_at(X0,Y0),
    distance1((X0,Y0),(X1,Y1),D1),
    distance1((X0,Y0),(X,Y),D2),
    D1<D2.
l([goal(X1,Y1),A],[[goal(X,Y),B]|[]],[[goal(X,Y),B],[goal(X1,Y1),A]]):-
    agent_at(X0,Y0),
    distance1((X0,Y0),(X1,Y1),D1),
    distance1((X0,Y0),(X,Y),D2),
    D1>=D2.
l([goal(X1,Y1),A],[[goal(X,Y),B]|T],[[goal(X1,Y1),A],[goal(X,Y),B]|T]):-
    agent_at(X0,Y0),
    distance1((X0,Y0),(X1,Y1),D1),
    distance1((X0,Y0),(X,Y),D2),
    D1<D2.

l([goal(X1,Y1),A],[[goal(X,Y),B],[goal(X2,Y2),C]|T],[[goal(X,Y),B],[goal(X1,Y1),A],[goal(X2,Y2),C]|T]):-
    agent_at(X0,Y0),
    distance1((X0,Y0),(X1,Y1),D1),
    distance1((X0,Y0),(X,Y),D2),
    distance1((X0,Y0),(X2,Y2),D3),
    D1>=D2,D1<D3.
l([goal(X1,Y1),A],[[goal(X,Y),B],[goal(X2,Y2),C]|T],[[goal(X,Y),B],[goal(X2,Y2),C]|T1]):-
    agent_at(X0,Y0),
    distance1((X0,Y0),(X1,Y1),D1),
    distance1((X0,Y0),(X,Y),D2),
    distance1((X0,Y0),(X2,Y2),D3),
    D1>=D2,D1>=D3,
    l([goal(X1,Y1),A],[[goal(X2,Y2),C]|T],[[goal(X2,Y2),C]|T1]).


% r() is defined to select every stone which can be accessed on land or
% dropped, and the stone should be the new one which haven't included in
% the Intpick queue, and we use l() to make the Intpick queue to be the
% decreasing order of the length of path.
r([[goal(X,Y),A]|T],L,R):-
   solve((X,Y),_,_,_,3),
    not(member(L,[goal(X,Y),_])),
    l([goal(X,Y),A],L,L1),
    r(T,L1,R).
r([[goal(X,Y),A]|T],L,R):-
    agent_at(X,Y),
    not(member(L,[goal(X,Y),_])),
    l([goal(X,Y),A],L,L1),
    r(T,L1,R).
r([[goal(_,_),_]|T],L,R):-
    r(T,L,R).
r([],L,L).

% This is a special case which represents when there are no new stones
% appear, we just to copy the old intentions.
incorporate_goals([],Intentions,Intentions).
% Otherwise, we need to use r() to select all the proper stones and
% update the new intention.
incorporate_goals(Goals, intents(L,A), intents(L,A1)):-
	trigger2(Goals,Newgoal),
	r(Newgoal,A,A1).


%Q4
% When the goal(X,Y) is the same as the current location of agent and
% the agent haven't got a stone, we need to design a path (length 2:
% move + pick)
get_action(intents(Int_drop,[[goal(X,Y),[]]|H1]),intents(Int_drop,[[goal(X,Y),[pick(X,Y)]]|H1]),move(X1,Y1)):-
	agent_stones(0),
	agent_at(X,Y),
	p3((X,Y),(X1,Y1)).
% when the agent holding a stone and the 1st point intents of drop is
% null, we design a new path to drop the stone and put out the 1st
% percepts as an action.
get_action(intents([[goal(X,Y),[]]|H1],Int_pick),intents([[goal(X,Y),T]|H1],Int_pick),H):-
        agent_stones(1),
	solve((X,Y),Solution,_,_,3),
	transfer(Solution,C),
	change4(C,[H|T]).
% If there exists some intents of the Intdrop, we just need to copy the
% 1st percept in the queue as an action.
get_action(intents([[goal(X,Y),[H|T]]|H1],Int_pick),intents([[goal(X,Y),T]|H1],Int_pick), H):-
    agent_stones(1).

% if there are no stones holding by the agent, and the percept
% of the 1st point of Intpick is null, design a new path to got the
% stone.
get_action(intents(Int_drop,[[goal(X,Y),[]]|H1]),intents(Int_drop,[[goal(X,Y),T]|H1]),H):-
    agent_stones(0),
	solve((X,Y),Solution,_,_,3),
	transfer1(Solution,C),
	change4(C,[H|T]).
% if the percept of the 1st point of Intpick is not null, copy the 1st
% percepts as an action.
get_action(intents(Int_drop,[[goal(X,Y),[H|T]]|H1]),intents(Int_drop,[[goal(X,Y),T]|H1]), H):-
	agent_stones(0).
% If there are no pick goals, the agent just stay at the same point to
% wait for the new goal.
get_action(intents(A,[]), intents(A,[]), move(X,Y)):-
	agent_stones(0),
	agent_at(X,Y).

% these two function is defined to change the format of the path, from
% (X,Y)->move(X,Y),and the last goal of the path be drop(X,Y) or
% pick(X,Y).
transfer([],[]).
transfer([(X,Y)],[drop(X,Y)]).
transfer([(X,Y)|T],[move(X,Y)|T1]):-
	transfer(T,T1).
transfer1([],[]).
transfer1([(X,Y)],[pick(X,Y)]).
transfer1([(X,Y)|T],[move(X,Y)|T1]):-
	transfer1(T,T1).
change4([_|T],T).
%Q5
% if Oberservation is drop, remove the goal from int_drop in the
% intentions.
update_intentions(Observation, intents([[goal(X,Y),_]|T],A),intents(T,A)):-
    Observation = dropped(X,Y).

% if Oberservation is pick, remove the goal from int_pick in the
% intentions.
update_intentions(Observation, intents(A,[[goal(X,Y),_]|T]), intents(A,T)):-
    Observation = picked(X,Y).
%Else just remain the intention as same as the former intention.
update_intentions(_,X,X).






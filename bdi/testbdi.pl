/*------------------------------------------------------------------------------
% Test scenario for BDI system
% Example of what we need to provide to make the BDI runs
% Auteur:
% Date: 2013-04-14
------------------------------------------------------------------------------*/

:- use_module(bdi).
:- use_module('../pathfinding/findpath').
:- multifile plan/3.

/*------------------------------------------------------------------------------
 Action prototypes for testing
 Actions are things agents do in the real-world context
 Hypothesis : same actions definitions for agents and the player
------------------------------------------------------------------------------*/
poss(move(Actor, From, To),
 [isa_actor(Actor), at(Actor, From), path(From, To, _)],
 [at(Actor, To)],
 [at(Actor, From)]).

% take e.g. a prop
poss(take(Actor, Prop, Food),
 [isa_actor(Actor), at(Actor, X), has(Prop, Food), isa_food(Food, _), at(Prop, X), is_visible(Food)],
 [has(Actor, Food)],
 [has(Prop, Food)]).

% eat a prop
poss(eat(Actor, Prop),
 [isa_actor(Actor), isa_food(Prop, Food), has(Actor, Prop)],
 [add(health(Actor), Food)],
 [has(Actor, Prop)]).

% grow = get new fruit
poss(grow(Actor, Fruit), [not(has(Actor, Fruit))], [has(Actor, Fruit)], []).

/*------------------------------------------------------------------------------
 Desires declarations with context for testing
 Desires are things agents are willing to do regarding their beliefs
 Specific to agents
------------------------------------------------------------------------------*/
desire(eat(Actor, Prop),
 [isa_actor(Actor), health(Actor, X), X < 50, has(Actor, Prop), isa_food(Prop, Food), Food > 0]).

desire(getfood(Actor, From, Prop),
 [isa_actor(Actor), health(Actor, X), X < 50, isa_food(Prop, Food), Food > 0, has(From, Prop)]).

desire(explore(Actor), [isa_actor(Actor)]).

% example of a very simple desire, wich can be directly executed as an action "grow"
desire(grow(Actor, banana), [not(has(Actor, banana))]).

/*------------------------------------------------------------------------------
Plan library for test:
 - generate a plan e.g. using pathfinding
 - simply turn it to an action (default)
 - TODO : list of actions => description language?
------------------------------------------------------------------------------*/

% FINDFOOD
plan(getfood(Agent, Prop, Food), B, Plan) :-
 member(at(Agent, From), B),
 member(at(Prop, To), B),
 findpath:findpath(From, To, B, [_|NewPath], _),
 path2move(Agent, From, NewPath, Temp),
 append(Temp, [take(Agent, Prop, Food)], Plan),
 format("Plan: ~w~n", [Plan]).

% EXPLORE
plan(explore(Agent), B, Plan) :-
 member(at(Agent, From), B),
 findall(X, ((member(isa_region(X), B) ; member(isa_room(X), B)), X \== From), L),
 length(L, Length),
 random(0, Length, Index),
 nth0(Index, L, To),
 findpath:findpath(From, To, B, [_|NewPath], _),
 path2move(Agent, From, NewPath, Plan),
 format("Plan: ~w~n", [Plan]).

path2move(_, _, [], []).
path2move(Agent, From, [H|T], [move(Agent, From, H)|P]) :- path2move(Agent, H, T, P).

% DEFAULT
% Plan implementation: re-inject Intention as an Action
% ex: plan(eat(Agent, Food), _, [eat(Agent, Food)]).
plan(I, _, [I]).

/*------------------------------------------------------------------------------
 Unit tests
------------------------------------------------------------------------------*/
:- begin_tests(bdi).

test(1) :-
 S = [
  isa_region(r1),
  isa_region(r2),
  isa_region(r3),
  isa_region(r4),
  isa_region(r5),

  path(r1, r2, 1),
  path(r1, r4, 1),
  path(r1, r5, 2),

  path(r2, r1, 1),
%  path(r2, r3, 1),
  path(r2, r5, 2),

  path(r3, r2, 1),
  path(r3, r4, 1),
  path(r3, r5, 2),

  path(r4, r3, 1),
  path(r4, r1, 1),
  path(r4, r5, 2),

  path(r5, r1, 2),
  path(r5, r2, 2),
  path(r5, r3, 2),
  path(r5, r4, 2),

  isa_prop(banana),
  isa_food(banana, 10),
  is_visible(banana),

  isa_prop(tree),
  at(tree, r3),
  is_visible(tree),
  has(tree, banana),
  agent(tree, [], [grow(_,_)], [], []),

  isa_actor(monkey),
  is_visible(monkey), % IMPORTANT NOW TO BE PERCEIVED!!!
  health(monkey, 40),
  at(monkey, r1),
%  has(monkey, banana),
  agent(monkey,
   % initial beliefs
   [
    isa_region(r1), isa_region(r2), isa_region(r3), isa_region(r4), isa_region(r5),
    path(r1, r2, 1), path(r1, r4, 1), path(r1, r5, 2),
    path(r2, r1, 1), path(r2, r3, 1), path(r2, r5, 2),
    path(r3, r2, 1), path(r3, r4, 1), path(r3, r5, 2),
    path(r4, r3, 1), path(r4, r1, 1), path(r4, r5, 2),
    path(r5, r1, 2), path(r5, r2, 2), path(r5, r3, 2), path(r5, r4, 2),

  isa_prop(banana),
  isa_food(banana, 10),
  is_visible(banana),
  isa_prop(tree),
  at(tree, r3),
  is_visible(tree),
  has(tree, banana)

    ],
   % initial list of possible desires
   [eat(_,_), getfood(_,_,_), explore(_)],
   % initial intention
   [],
   % plan is empty at start
   [])

/*
  isa_actor(gorilla),
  at(gorilla, r1),
  agent(gorilla,
   % initial beliefs
   [isa_region(r1), isa_region(r2), isa_region(r3), isa_region(r4), isa_region(r5),
    path(r1, r2, 1), path(r1, r4, 1), path(r1, r5, 2),
    path(r2, r1, 1), path(r2, r3, 1), path(r2, r5, 2),
    path(r3, r2, 1), path(r3, r4, 1), path(r3, r5, 2),
    path(r4, r3, 1), path(r4, r1, 1), path(r4, r5, 2),
    path(r5, r1, 2), path(r5, r2, 2), path(r5, r3, 2), path(r5, r4, 2)],
   % initial list of possible desires
   [explore(_)],
   % initial intention
   [],
   % plan is empty at start
   [])
*/

 ],
 format("~n"),
 testBDI(10, S, S1),
 format("END SITUATION = ~w~n", [S1]), !.

testBDI(0, S, S).
testBDI(N, S, S1) :-
 format("--------------------------------------------------------------------~n"),
 format("Iteration ~w:~n", [N]),
 bdi:prs(S, Temp),
 format("S = ~w~n", [Temp]),
 N1 is N - 1,
 testBDI(N1, Temp, S1).

:- end_tests(bdi).
:- run_tests(bdi).

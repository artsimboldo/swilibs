/*------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  BDI implementation in SC
  Auteur: rv
  ------------------------------------------------------------------------------
  HISTORY
  31-03-2013: prs foundations
  10-04-2013: brf v1, update and perceive functions.
  14-04-2013: basic option and filter, plan library (generated plan)
  17-04-2013: blind comitment, add plan validation during plan execution
  19-04-2013: internal variable for Health(Agent, Value)
  20-04-2013: unify variables in desire's contexts, default plan

  TODO:
  - add suceedeed predicate
  - process death
  - internal variables for vision, metabolism
  - executing sub-goals instead of actions
  - reconsidering intention during plan execution?
  - communication / message system
  - cooperation between agents
  - reasonning about beliefs
  ------------------------------------------------------------------------------
  API:
 -----------------------------------------------------------------------------*/
:- module(bdi, [prs/2]).
/*------------------------------------------------------------------------------
  dependencies
------------------------------------------------------------------------------*/
:- use_module('../sc/fluents',[holds/2]).
:- use_module('../sc/effects', [replaces/4, adds/3, dels/3]).
:- use_module(brf, [update/4]).
/*------------------------------------------------------------------------------
 ------------------------------------------------------------------------------
 -----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
 PRS : Procedural Reasonning System
------------------------------------------------------------------------------*/
prs(S, S1) :-
 findall(Agent, member(agent(Agent, _, _, _, _), S), L),
 random_permutation(L, Agents),
% format("Found agents : ~w~n", [Agents]),
 prsAgent(Agents, S, S1).

prsAgent([], S, S).

prsAgent([Agent|T], S, S1) :-
% findall(Des, (plan(Agent, Des, Context), holds(Context, S)), Desires),
 member(agent(Agent, B0, D0, I0, P0), S),
% format("Agent ~w, B0 = ~w, D0 = ~w, I0 =~w~n", [Agent, B0, D0, I0]),
% format("--------------------------------------------------------------------~n"),
 format("(...)~nAGENT ~w:~n", [Agent]),
 executePlan(Agent, B0, D0, I0, P0, S, Temp),
 prsAgent(T, Temp, S1).

% Determine plan
executePlan(Agent, B, D, I, [], S, S1) :-
 brf:update(Agent, B, NewB, S),
 options(Agent, D, NewB, I, NewD),
 (NewD \== [] ->
  (filter(NewB, NewD, I, NewI),
  plan(NewI, NewB, Plan),
  % execute first action
  executePlan(Agent, NewB, D, NewI, Plan, S, S1)) ;
  % if no options -> just update with new beliefs
  replaces(S, agent(Agent, _, _, _, _), agent(Agent, NewB, D, I, []), S1)).

% Commitment strategies goes here:
% Blind commitment
% other possibilities:
% - Single-minded commitment
% - Open-minded commitment
executePlan(Agent, B, D, I, [H|T], S, S1) :-
 % there is a plan, check first the plan hasn't succeeded yet
 not(succeeded(I, B)),
 executeAction(H, S, Temp),
 brf:update(Agent, B, NewB, Temp),
 (not(sound(T, I, NewB)) ->
  format("Execute Plan: plan not sound, new plan is:~n"),
  plan(I, NewB, Plan);
  Plan = T
 ),
 %format("Beliefs = ~w~n", [NewB]),
 replaces(Temp, agent(Agent, _, _, _, _), agent(Agent, NewB, D, I, Plan), S1).

% Handle plan failure
executePlan(Agent, B, D, I, Plan, S, S1) :-
 replaces(S, agent(Agent, _, _, _, _), agent(Agent, B, D, I, []), S1),
 format("Plan: ~w has failed~n", [Plan]).

/*------------------------------------------------------------------------------
 Succeeded: TODO
------------------------------------------------------------------------------*/
succeeded(I, B) :- fail.

/*------------------------------------------------------------------------------
 Sound: check if a plan is sound, simulates all actions updating beliefs
 return true if plan is sound
------------------------------------------------------------------------------*/
sound([], _, _).
sound([H|T], I, B) :-
 poss(H, Pr, Ad, Dl),
% format("sound: checking H = ~w, Pr = ~w, B = ~w~n", [H, Pr, B]),
 holds(Pr, B),
 dels(Dl, B, Temp),
 adds(Ad, Temp, B1),
 sound(T, I, B1).

/*------------------------------------------------------------------------------
 options :
  Select possible desires (i.e. where context is true in B) from the set of
  available desires D. Result in D1.
  Current intention I as a parameter avoid to consider antagonistic intentions
------------------------------------------------------------------------------*/
%options(D, B, I, D1) :-
options(Agent, D, B, _, D1) :-
% find all desires in D where preconditions Context are true in B
% format("options: ~w~n", [B]),
 findall(X, option(Agent, D, B, X), D1),
 format("Options: '~w' possibles~n", [D1]).

option(Agent, D, B, X) :-
 desire(X, Ctx),
% format("option: ~w ~w~n", [X, Ctx]),
 member(X, D),
 arg(1, X, Agent),
 holds(Ctx, B).

/*------------------------------------------------------------------------------
 filter(B, D, I, I1):
  Select intention that we'll commit to.
  Simplest strategy is to take the first of the list, remember D is an ordered
  set of desires (from preferred to default)
  Another (and better?) idea is to use Ockham's razor: estimate effort and take minimum
------------------------------------------------------------------------------*/
filter(_, D, _, I1) :-
 nth0(0, D, I1),
 format("Filter: '~w' selected~n", [I1]).

/*------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
executeAction(A, S, S1) :-
 poss(A, Pr, Ad, Dl),
 holds(Pr, S),
 dels(Dl, S, Temp),
 adds(Ad, Temp, Temp1),
 metabolism(A, Temp1, S1),
 format("Execute Action '~w'~n", [A]).

/*------------------------------------------------------------------------------
 Metabolism decrement health if exists
------------------------------------------------------------------------------*/
metabolism(A, S, S1) :-
 arg(1, A, Agent),
 member(health(Agent, H), S), !,
 NewH is H - 1,
 (NewH > 0 ->
  (replaces(S, health(Agent, H), health(Agent, NewH), S1));
  % Agent dead! TODO
  format("Metabolism: AGENT DEAD!!!~n"),
  S1 = S).
metabolism(_, S, S).

/*------------------------------------------------------------------------------
 Unit tests
 see testbdi.pl
------------------------------------------------------------------------------*/
/*
:- begin_tests(bdi).
test(1) :- !.
:- end_tests(bdi).
:- run_tests(bdi).
*/